package zio.json

import zio.json.ast.Json
import scala.annotation.*
import magnolia1.*
import scala.deriving.Mirror
import scala.compiletime.*
import scala.reflect.*
import zio.Chunk

import zio.json.JsonDecoder.JsonError
import zio.json.ast.Json
import zio.json.internal.{ Lexer, RetractReader, StringMatrix, Write }

import scala.annotation._
import scala.collection.mutable
import scala.language.experimental.macros

/**
 * If used on a case class field, determines the name of the JSON field.
 * Defaults to the case class field name.
 */
final case class jsonField(name: String) extends Annotation

/**
 * If used on a case class field, determines the alternative names of the JSON field.
 */
final case class jsonAliases(alias: String, aliases: String*) extends Annotation

/**
 * Empty option fields will be encoded as `null`.
 */
final class jsonExplicitNull extends Annotation

/**
 * When disabled keys with empty collections will be omitted from the JSON.
 */
final case class jsonExplicitEmptyCollection(enabled: Boolean = true) extends Annotation

/**
 * If used on a sealed class, will determine the name of the field for
 * disambiguating classes.
 *
 * The default is to not use a typehint field and instead
 * have an object with a single key that is the class name.
 *
 * Note that using a discriminator is less performant, uses more memory, and may
 * be prone to DOS attacks that are impossible with the default encoding. In
 * addition, there is slightly less type safety when using custom product
 * encoders (which must write an unenforced object type). Only use this option
 * if you must model an externally defined schema.
 */
final case class jsonDiscriminator(name: String) extends Annotation
// TODO a strategy where the constructor is inferred from the field names, only
// valid if there is no ambiguity in the types of fields for all case classes.
// Such a strategy cannot be implemented with Magnolia because the SealedTrait
// does not provide a mechanism for obtaining the CaseClass associated to the
// Subtype.

sealed trait JsonMemberFormat extends (String => String)

case class CustomCase(f: String => String) extends JsonMemberFormat {
  override def apply(memberName: String): String = f(memberName)
}
case object SnakeCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceSnakeOrKebabCase(memberName, '_')
}
case object CamelCase extends JsonMemberFormat {
  override def apply(memberName: String): String =
    jsonMemberNames.enforceCamelOrPascalCase(memberName, toPascal = false)
}

case object PascalCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceCamelOrPascalCase(memberName, toPascal = true)
}
case object KebabCase extends JsonMemberFormat {
  override def apply(memberName: String): String = jsonMemberNames.enforceSnakeOrKebabCase(memberName, '-')
}
case object IdentityFormat extends JsonMemberFormat {
  override def apply(memberName: String): String = memberName
}

/** zio-json version 0.3.0 formats. abc123Def -> abc_123_def */
object ziojson_03 {
  case object SnakeCase extends JsonMemberFormat {
    override def apply(memberName: String): String =
      jsonMemberNames.enforceSnakeOrKebabCaseSeparateNumbers(memberName, '_')
  }
  case object KebabCase extends JsonMemberFormat {
    override def apply(memberName: String): String =
      jsonMemberNames.enforceSnakeOrKebabCaseSeparateNumbers(memberName, '-')
  }
}

/**
 * If used on a case class, determines the strategy of member names
 * transformation during serialization and deserialization. Four common
 * strategies are provided above and a custom one to support specific use cases.
 */
final case class jsonMemberNames(format: JsonMemberFormat) extends Annotation
private[json] object jsonMemberNames {

  /**
   * ~~Stolen~~ Borrowed from jsoniter-scala by Andriy Plokhotnyuk
   * (he even granted permission for this, imagine that!)
   */

  import java.lang.Character._

  def enforceCamelOrPascalCase(s: String, toPascal: Boolean): String =
    if (s.indexOf('_') == -1 && s.indexOf('-') == -1) {
      if (s.isEmpty) s
      else {
        val ch = s.charAt(0)
        val fixedCh =
          if (toPascal) toUpperCase(ch)
          else toLowerCase(ch)
        s"$fixedCh${s.substring(1)}"
      }
    } else {
      val len             = s.length
      val sb              = new StringBuilder(len)
      var i               = 0
      var isPrecedingDash = toPascal
      while (i < len) isPrecedingDash = {
        val ch = s.charAt(i)
        i += 1
        (ch == '_' || ch == '-') || {
          val fixedCh =
            if (isPrecedingDash) toUpperCase(ch)
            else toLowerCase(ch)
          sb.append(fixedCh)
          false
        }
      }
      sb.toString
    }

  def enforceSnakeOrKebabCase(s: String, separator: Char): String = {
    val len                      = s.length
    val sb                       = new StringBuilder(len << 1)
    var i                        = 0
    var isPrecedingNotUpperCased = false
    while (i < len) isPrecedingNotUpperCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        sb.append(separator)
        false
      } else if (!isUpperCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingNotUpperCased || i > 1 && i < len && !isUpperCase(s.charAt(i))) sb.append(separator)
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }

  def enforceSnakeOrKebabCaseSeparateNumbers(s: String, separator: Char): String = {
    val len                   = s.length
    val sb                    = new StringBuilder(len << 1)
    var i                     = 0
    var isPrecedingLowerCased = false
    while (i < len) isPrecedingLowerCased = {
      val ch = s.charAt(i)
      i += 1
      if (ch == '_' || ch == '-') {
        sb.append(separator)
        false
      } else if (isLowerCase(ch)) {
        sb.append(ch)
        true
      } else {
        if (isPrecedingLowerCased || i > 1 && i < len && isLowerCase(s.charAt(i))) sb.append(separator)
        sb.append(toLowerCase(ch))
        false
      }
    }
    sb.toString
  }

}

/**
 * If used on a case class will determine the type hint value for disambiguating
 * sealed traits. Defaults to the short type name.
 */
final case class jsonHint(name: String) extends Annotation

/**
 * If used on a sealed class will determine the strategy of type hint value transformation for disambiguating
 * classes during serialization and deserialization. Same strategies are provided as for [[jsonMemberNames]].
 */
final case class jsonHintNames(format: JsonMemberFormat) extends Annotation

/**
 * If used on a case class, will exit early if any fields are in the JSON that
 * do not correspond to field names in the case class.
 *
 * This adds extra protections against a DOS attacks but means that changes in
 * the schema will result in a hard error rather than silently ignoring those
 * fields.
 *
 * Cannot be combined with `@jsonDiscriminator` since it is considered an extra
 * field from the perspective of the case class.
 */
final class jsonNoExtraFields extends Annotation

/**
 * If used on a case class field, will exclude it from the resulting JSON.
 */
final class jsonExclude extends Annotation

private class CaseObjectDecoder[Typeclass[*], A](val ctx: CaseClass[Typeclass, A], no_extra: Boolean) extends JsonDecoder[A] {
  def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
    if (no_extra) {
      Lexer.char(trace, in, '{')
      Lexer.char(trace, in, '}')
    } else Lexer.skipValue(trace, in)
    ctx.rawConstruct(Nil)
  }

  override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
    json match {
      case Json.Obj(_) => ctx.rawConstruct(Nil)
      case Json.Null   => ctx.rawConstruct(Nil)
      case _           => Lexer.error("Not an object", trace)
    }
}

sealed class JsonDecoderDerivation(config: JsonCodecConfiguration) extends Derivation[JsonDecoder] { self =>
  def join[A](ctx: CaseClass[Typeclass, A]): JsonDecoder[A] = {
    val (transformNames, nameTransform): (Boolean, String => String) =
      ctx.annotations.collectFirst { case jsonMemberNames(format) => format }
        .orElse(Some(config.fieldNameMapping))
        .filter(_ != IdentityFormat)
        .map(true -> _)
        .getOrElse(false -> identity)

    val no_extra = ctx
      .annotations
      .collectFirst { case _: jsonNoExtraFields => () }
      .isDefined || !config.allowExtraFields

    if (ctx.params.isEmpty) {
      new CaseObjectDecoder(ctx, no_extra)
    } else {
      new JsonDecoder[A] {
        private val (names, aliases): (Array[String], Array[(String, Int)]) = {
          val names = Array.ofDim[String](ctx.params.size)
          val aliasesBuilder = Array.newBuilder[(String, Int)]
          ctx.params.foreach {
            var idx = 0
            p =>
              names(idx) = p
                .annotations
                .collectFirst { case jsonField(name) => name }
                .getOrElse(if (transformNames) nameTransform(p.label) else p.label)
              aliasesBuilder ++= p
                .annotations
                .flatMap {
                  case jsonAliases(alias, aliases*) => (alias +: aliases).map(_ -> idx)
                  case _ => Seq.empty
                }
              idx += 1
          }
          val aliases = aliasesBuilder.result()
          val allFieldNames = names ++ aliases.map(_._1)
          if (allFieldNames.length != allFieldNames.distinct.length) {
            val aliasNames = aliases.map(_._1)
            val collisions = aliasNames
              .filter(alias => names.contains(alias) || aliases.count { case (a, _) => a == alias } > 1)
              .distinct
            val msg = s"Field names and aliases in case class ${ctx.typeInfo.full} must be distinct, " +
              s"alias(es) ${collisions.mkString(",")} collide with a field or another alias"
            throw new AssertionError(msg)
          }
          (names, aliases)
        }
        private val len = names.length
        private val matrix = new StringMatrix(names, aliases)
        private val spans = names.map(JsonError.ObjectAccess(_))
        private val defaults = IArray.genericWrapArray(ctx.params.map(_.default)).toArray
        private lazy val tcs =
          IArray.genericWrapArray(ctx.params.map(_.typeclass)).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
        private lazy val namesMap = (names.zipWithIndex ++ aliases).toMap

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')
          val ps = new Array[Any](len)
          if (Lexer.firstField(trace, in))
            while({
              val idx = Lexer.field(trace, in, matrix)
              if (idx != -1) {
                if (ps(idx) != null) Lexer.error("duplicate", trace)
                val default = defaults(idx)
                ps(idx) = if ((default eq None) || in.nextNonWhitespace() != 'n' && {
                  in.retract()
                  true
                }) tcs(idx).unsafeDecode(spans(idx) :: trace, in)
                else if (in.readChar() == 'u' && in.readChar() == 'l' && in.readChar() == 'l') default.get
                else Lexer.error("expected 'null'", spans(idx) :: trace)
              } else if (no_extra) Lexer.error("invalid extra field", trace)
              else Lexer.skipValue(trace, in)
              Lexer.nextField(trace, in)
            }) ()
          var idx = 0
          while (idx < len) {
            if (ps(idx) == null) {
              val default = defaults(idx)
              ps(idx) =
                if (default ne None) default.get
                else tcs(idx).unsafeDecodeMissing(spans(idx) :: trace)
            }
            idx += 1
          }
          ctx.rawConstruct(new ArraySeq(ps))
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          json match {
            case Json.Obj(keyValues) =>
              val ps = new Array[Any](len)
              for ((key, value) <- keyValues) {
                namesMap.get(key) match {
                  case Some(idx) =>
                    if (ps(idx) != null) Lexer.error("duplicate", trace)
                    val default = defaults(idx)
                    ps(idx) =
                      if ((default ne None) && (value eq Json.Null)) default.get
                      else tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, value)
                  case _ =>
                    if (no_extra) Lexer.error("invalid extra field", trace)
                }
              }
              var idx = 0
              while (idx < len) {
                if (ps(idx) == null) {
                  val default = defaults(idx)
                  ps(idx) =
                    if (default ne None) default.get
                    else tcs(idx).unsafeDecodeMissing(spans(idx) :: trace)
                }
                idx += 1
              }
              ctx.rawConstruct(new ArraySeq(ps))
            case _ => Lexer.error("Not an object", trace)
          }
      }
    }
  }

  def split[A](ctx: SealedTrait[JsonDecoder, A]): JsonDecoder[A] = {
    val jsonHintFormat: JsonMemberFormat =
      ctx.annotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(config.sumTypeMapping)
    val names: Array[String] = IArray.genericWrapArray(ctx.subtypes.map { p =>
      p.annotations.collectFirst { case jsonHint(name) => name }.getOrElse(jsonHintFormat(p.typeInfo.short))
    }).toArray
    val matrix: StringMatrix = new StringMatrix(names)
    lazy val tcs: Array[JsonDecoder[Any]] =
      IArray.genericWrapArray(ctx.subtypes.map(_.typeclass)).toArray.asInstanceOf[Array[JsonDecoder[Any]]]
    lazy val namesMap: Map[String, Int] = names.zipWithIndex.toMap

    def isEnumeration = 
      (ctx.isEnum && ctx.subtypes.forall(_.typeclass.isInstanceOf[CaseObjectDecoder[?, ?]])) || (
        !ctx.isEnum && ctx.subtypes.forall(_.isObject)
      )

    def discrim =
      ctx.annotations.collectFirst { case jsonDiscriminator(n) => n }.orElse(config.sumTypeHandling.discriminatorField)

    if (isEnumeration && discrim.isEmpty) {
      new JsonDecoder[A] {
        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          val idx = Lexer.enumeration(trace, in, matrix)
          if (idx != -1) tcs(idx).asInstanceOf[CaseObjectDecoder[JsonDecoder, A]].ctx.rawConstruct(Nil)
          else Lexer.error("Invalid enumeration value", trace)
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          json match {
            case Json.Str(typeName) => namesMap.get(typeName) match {
              case Some(idx) => tcs(idx).asInstanceOf[CaseObjectDecoder[JsonDecoder, A]].ctx.rawConstruct(Nil)
              case _         => Lexer.error("Invalid enumeration value", trace)
            }
            case _ => Lexer.error("Not a string", trace)
          }
      }
    } else if (discrim.isEmpty) {
      // We're not allowing extra fields in this encoding
      new JsonDecoder[A] {
        private val spans: Array[JsonError] = names.map(JsonError.ObjectAccess(_))

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          Lexer.char(trace, in, '{')
          if (Lexer.firstField(trace, in)) {
            val idx = Lexer.field(trace, in, matrix)
            if (idx != -1) {
              val a = tcs(idx).unsafeDecode(spans(idx) :: trace, in).asInstanceOf[A]
              Lexer.char(trace, in, '}')
              a
            } else Lexer.error("invalid disambiguator", trace)
          } else Lexer.error("expected non-empty object", trace)
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = {
          json match {
            case Json.Obj(chunk) if chunk.size == 1 =>
              val keyValue = chunk.head
              namesMap.get(keyValue._1) match {
                case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, keyValue._2).asInstanceOf[A]
                case _         => Lexer.error("Invalid disambiguator", trace)
              }
            case Json.Obj(_) => Lexer.error("Not an object with a single field", trace)
            case _           => Lexer.error("Not an object", trace)
          }
        }
      }
    } else {
      new JsonDecoder[A] {
        private val hintfield = discrim.get
        private val hintmatrix = new StringMatrix(Array(hintfield))
        private val spans = names.map(JsonError.Message(_))

        def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
          val in_ = zio.json.internal.RecordingReader(in)
          Lexer.char(trace, in_, '{')
          if (Lexer.firstField(trace, in_)) {
            while ({
              if (Lexer.field(trace, in_, hintmatrix) != -1) {
                val idx = Lexer.enumeration(trace, in_, matrix)
                if (idx == -1) Lexer.error("invalid disambiguator", trace)
                in_.rewind()
                return tcs(idx).unsafeDecode(spans(idx) :: trace, in_).asInstanceOf[A]
              } else Lexer.skipValue(trace, in_)
              Lexer.nextField(trace, in_)
            }) ()
          }
          Lexer.error(s"missing hint '$hintfield'", trace)
        }

        override final def unsafeFromJsonAST(trace: List[JsonError], json: Json): A = {
          json match {
            case Json.Obj(fields) =>
              fields.find { case (key, _) => key == hintfield } match {
                case Some((_, Json.Str(name))) => namesMap.get(name) match {
                  case Some(idx) => tcs(idx).unsafeFromJsonAST(spans(idx) :: trace, json).asInstanceOf[A]
                  case _      => Lexer.error("Invalid disambiguator", trace)
                }
                case Some(_) => Lexer.error(s"Non-string hint '$hintfield'", trace)
                case _ => Lexer.error(s"Missing hint '$hintfield'", trace)
              }
            case _ => Lexer.error("Not an object", trace)
          }
        }
      }
    }
  }

  inline def gen[A](using mirror: Mirror.Of[A]) = self.derived[A]

  // Backcompat for 2.12, otherwise we'd use ArraySeq.unsafeWrapArray
  private final class ArraySeq(p: Array[Any]) extends IndexedSeq[Any] {
    def apply(i: Int): Any = p(i)
    def length: Int        = p.length
  }
}

private lazy val caseObjectEncoder = new JsonEncoder[Any] {
  def unsafeEncode(a: Any, indent: Option[Int], out: Write): Unit =
    out.write("{}")

  override final def toJsonAST(a: Any): Either[String, Json] =
    Right(Json.Obj(Chunk.empty))
}

object DeriveJsonDecoder extends JsonDecoderDerivation(JsonCodecConfiguration.default) { self =>
  inline def gen[A](using config: JsonCodecConfiguration, mirror: Mirror.Of[A]) = {
    val derivation = new JsonDecoderDerivation(config)
    derivation.derived[A]
  }

  // Backcompat for 2.12, otherwise we'd use ArraySeq.unsafeWrapArray
  private final class ArraySeq(p: Array[Any]) extends IndexedSeq[Any] {
    def apply(i: Int): Any = p(i)
    def length: Int        = p.length
  }
}

sealed class JsonEncoderDerivation(config: JsonCodecConfiguration) extends Derivation[JsonEncoder] { self =>
   def join[A](ctx: CaseClass[Typeclass, A]): JsonEncoder[A] =
    if (ctx.params.isEmpty) {
      caseObjectEncoder.narrow[A]
    } else {
      new JsonEncoder[A] {
        private val (transformNames, nameTransform): (Boolean, String => String) = ctx.annotations
          .collectFirst { case jsonMemberNames(format) => format }
          .orElse(Some(config.fieldNameMapping))
          .filter(_ != IdentityFormat)
          .map(true -> _)
          .getOrElse(false -> identity)
        private val params = IArray.genericWrapArray(ctx.params.filterNot { param =>
          param.annotations.collectFirst { case _: jsonExclude => () }.isDefined
        }).toArray
        private val names = params.map { p =>
          p.annotations.collectFirst {
            case jsonField(name) => name
          }.getOrElse(if (transformNames) nameTransform(p.label) else p.label)
        }.toArray
        private val explicitNulls = config.explicitNulls || ctx.annotations.exists(_.isInstanceOf[jsonExplicitNull])
        private val explicitEmptyCollections = ctx.annotations.collectFirst { case jsonExplicitEmptyCollection(enabled) =>
          enabled
        }.getOrElse(config.explicitEmptyCollections)
        private lazy val fields = params.map {
          var idx = 0
          p =>
            val field = (p, names(idx), p.typeclass.asInstanceOf[JsonEncoder[Any]],
              explicitNulls || p.annotations.exists(_.isInstanceOf[jsonExplicitNull]),
              p.annotations.collectFirst { case jsonExplicitEmptyCollection(enabled) =>
                enabled
              }.getOrElse(explicitEmptyCollections))
            idx += 1
            field
        }.toArray

        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          out.write('{')
          var indent_ = JsonEncoder.bump(indent)
          JsonEncoder.pad(indent_, out)
          val fields = this.fields
          var idx = 0
          var prevFields = false
          while (idx < fields.length) {
            val field = fields(idx)
            val p     = field._1.deref(a)
            if ({
              val isNothing = field._3.isNothing(p)
              val isEmpty = field._3.isEmpty(p)
              (!isNothing && !isEmpty) || (isNothing && field._4) || (isEmpty && field._5)
            }) {
              // if we have at least one field already, we need a comma
              if (prevFields) {
                out.write(',')
                JsonEncoder.pad(indent_, out)
              }
              JsonEncoder.string.unsafeEncode(field._2, indent_, out)
              if (indent.isEmpty) out.write(':')
              else out.write(" : ")
              field._3.unsafeEncode(p, indent_, out)
              prevFields = true // at least one field so far
            }
            idx += 1
          }
          JsonEncoder.pad(indent, out)
          out.write('}')
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          ctx.params
            .foldLeft[Either[String, Chunk[(String, Json)]]](Right(Chunk.empty)) { case (c, param) =>
              val name = param.annotations.collectFirst { case jsonField(name) =>
                name
              }.getOrElse(nameTransform(param.label))
              val writeNulls = explicitNulls || param.annotations.exists(_.isInstanceOf[jsonExplicitNull])
              val writeEmptyCollections =
                param.annotations.collectFirst { case jsonExplicitEmptyCollection(enabled) =>
                  enabled
                }.getOrElse(explicitEmptyCollections)
              c.flatMap { chunk =>
                param.typeclass.toJsonAST(param.deref(a)).map { value =>
                  if (
                    (value == Json.Null && !writeNulls) ||
                    (value.asObject.exists(_.fields.isEmpty) && !writeEmptyCollections)
                  ) chunk
                  else chunk :+ name -> value
                }
              }
            }
            .map(Json.Obj.apply)
        }
      }
    }

   def split[A](ctx: SealedTrait[JsonEncoder, A]): JsonEncoder[A] = {
    val isEnumeration = 
      (ctx.isEnum && ctx.subtypes.forall(_.typeclass == caseObjectEncoder)) || (
        !ctx.isEnum && ctx.subtypes.forall(_.isObject)
      )
    val jsonHintFormat: JsonMemberFormat =
      ctx.annotations.collectFirst { case jsonHintNames(format) => format }.getOrElse(config.sumTypeMapping)
    val discrim = ctx
      .annotations
      .collectFirst {
        case jsonDiscriminator(n) => n
      }.orElse(config.sumTypeHandling.discriminatorField)

    if (isEnumeration && discrim.isEmpty) {
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          val typeName = ctx.choose(a) { sub =>
            sub
              .annotations
              .collectFirst {
                case jsonHint(name) => name
              }.getOrElse(sub.typeInfo.short)
          }

          JsonEncoder.string.unsafeEncode(typeName, indent, out)
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          ctx.choose(a) { sub =>
            Right(
              Json.Str(
                sub
                  .annotations
                  .collectFirst {
                    case jsonHint(name) => name
                  }.getOrElse(sub.typeInfo.short)
              )
            )
          }
        }
      }
    } else if (discrim.isEmpty) {
      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          ctx.choose(a) { sub =>
            val name = sub
              .annotations
              .collectFirst {
                case jsonHint(name) => name
              }.getOrElse(jsonHintFormat(sub.typeInfo.short))
            out.write('{')
            val indent_ = JsonEncoder.bump(indent)
            JsonEncoder.pad(indent_, out)
            JsonEncoder.string.unsafeEncode(name, indent_, out)
            if (indent.isEmpty) out.write(':')
            else out.write(" : ")
            sub.typeclass.unsafeEncode(sub.cast(a), indent_, out)
            JsonEncoder.pad(indent, out)
            out.write('}')
          }
        }

        final override def toJsonAST(a: A): Either[String, Json] = {
          ctx.choose(a) { sub =>
            sub.typeclass.toJsonAST(sub.cast(a)).map { inner =>
              val name = sub
                .annotations
                .collectFirst {
                  case jsonHint(name) => name
                }.getOrElse(jsonHintFormat(sub.typeInfo.short))

              Json.Obj(
                Chunk(
                  name -> inner
                )
              )
            }
          }
        }
      }
    } else {
      val hintField = discrim.get

      def getName(annotations: Iterable[_], default: => String): String =
        annotations
          .collectFirst { case jsonHint(name) => name }
          .getOrElse(jsonHintFormat(default))

      new JsonEncoder[A] {
        def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
          ctx.choose(a) { sub =>
            out.write('{')
            val indent_ = JsonEncoder.bump(indent)
            JsonEncoder.pad(indent_, out)
            JsonEncoder.string.unsafeEncode(hintField, indent_, out)
            if (indent.isEmpty) out.write(':')
            else out.write(" : ")
            JsonEncoder.string.unsafeEncode(getName(sub.annotations, sub.typeInfo.short), indent_, out)
            // whitespace is always off by 2 spaces at the end, probably not worth fixing
            val intermediate = new DeriveJsonEncoder.NestedWriter(out, indent_)
            sub.typeclass.unsafeEncode(sub.cast(a), indent, intermediate)
          }
        }

        override final def toJsonAST(a: A): Either[String, Json] = {
          ctx.choose(a) { sub =>
            sub.typeclass.toJsonAST(sub.cast(a)).flatMap {
              case Json.Obj(fields) => Right(Json.Obj(fields :+ hintField -> Json.Str(getName(sub.annotations, sub.typeInfo.short))))
              case _                => Left("Subtype is not encoded as an object")
            }
          }
        }
      }
    }
  }
}

object DeriveJsonEncoder extends JsonEncoderDerivation(JsonCodecConfiguration.default) { self =>
  inline def gen[A](using config: JsonCodecConfiguration, mirror: Mirror.Of[A]) = {
    val derivation = new JsonEncoderDerivation(config)
    derivation.derived[A]
  }

  // intercepts the first `{` of a nested writer and discards it. We also need to
  // inject a `,` unless an empty object `{}` has been written.
  private[json] final class NestedWriter(out: Write, indent: Option[Int]) extends Write {
    private[this] var state = 2

    def write(c: Char): Unit =
      if (state != 0) {
        if (c == ' ' || c == '\n') {
          ()
        } else if (state == 2 && c == '{') {
          state = 1
        } else if (state == 1) {
          state = 0
          if (c != '}') {
            out.write(',')
            JsonEncoder.pad(indent, out)
          }
          out.write(c)
        }
      } else out.write(c)

    def write(s: String): Unit =
      if (state != 0) {
        var i = 0
        while (i < s.length) {
          val c = s.charAt(i)
          if (c == ' ' || c == '\n') {
            ()
          } else if (state == 2 && c == '{') {
            state = 1
          } else if (state == 1) {
            state = 0
            if (c != '}') {
              out.write(',')
              JsonEncoder.pad(indent, out)
            }
            while (i < s.length) {
              out.write(s.charAt(i))
              i += 1
            }
            return
          }
          i += 1
        }
      } else out.write(s)
  }
}

object DeriveJsonCodec {
  inline def gen[A](using mirror: Mirror.Of[A], config: JsonCodecConfiguration) = {
    val encoder = DeriveJsonEncoder.gen[A]
    val decoder = DeriveJsonDecoder.gen[A]

    JsonCodec(encoder, decoder)
  }
}
