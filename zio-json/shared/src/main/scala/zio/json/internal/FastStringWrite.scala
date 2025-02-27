package zio.json.internal

import java.nio.CharBuffer
import java.util.Arrays

final class FastStringWrite(initial: Int) extends Write {
  require(initial >= 8)
  private[this] var chars: Array[Char] = new Array[Char](initial)
  private[this] var count: Int         = 0

  @inline def reset(): Unit = count = 0

  def write(s: String): Unit = {
    val l = s.length
    val i = count
    if (i + l >= chars.length) chars = Arrays.copyOf(chars, Math.max(chars.length << 1, i + l))
    s.getChars(0, l, chars, i)
    count = i + l
  }

  def write(c: Char): Unit = {
    val i = count
    if (i + 1 >= chars.length) chars = Arrays.copyOf(chars, chars.length << 1)
    chars(i) = c
    count = i + 1
  }

  override def write(cs: Array[Char], from: Int, to: Int): Unit = {
    val from_ = count
    val len   = to - from
    if (from_ + len >= chars.length) chars = Arrays.copyOf(chars, Math.max(chars.length << 1, from_ + len))
    System.arraycopy(cs, from, chars, from_, len)
  }

  override def write(c1: Char, c2: Char): Unit = {
    var cs = chars
    val i  = count
    if (i + 1 >= cs.length) {
      cs = Arrays.copyOf(cs, cs.length << 1)
      chars = cs
    }
    cs(i) = c1
    cs(i + 1) = c2
    count = i + 2
  }

  override def write(c1: Char, c2: Char, c3: Char): Unit = {
    var cs = chars
    val i  = count
    if (i + 2 >= cs.length) {
      cs = Arrays.copyOf(cs, cs.length << 1)
      chars = cs
    }
    cs(i) = c1
    cs(i + 1) = c2
    cs(i + 2) = c3
    count = i + 3
  }

  override def write(c1: Char, c2: Char, c3: Char, c4: Char): Unit = {
    var cs = chars
    val i  = count
    if (i + 3 >= cs.length) {
      cs = Arrays.copyOf(cs, cs.length << 1)
      chars = cs
    }
    cs(i) = c1
    cs(i + 1) = c2
    cs(i + 2) = c3
    cs(i + 3) = c4
    count = i + 4
  }

  override def write(c1: Char, c2: Char, c3: Char, c4: Char, c5: Char): Unit = {
    var cs = chars
    val i  = count
    if (i + 4 >= cs.length) {
      cs = Arrays.copyOf(cs, cs.length << 1)
      chars = cs
    }
    cs(i) = c1
    cs(i + 1) = c2
    cs(i + 2) = c3
    cs(i + 3) = c4
    cs(i + 4) = c5
    count = i + 5
  }

  override def write(s: Short): Unit = {
    var cs = chars
    val i  = count
    if (i + 1 >= cs.length) {
      cs = Arrays.copyOf(cs, cs.length << 1)
      chars = cs
    }
    cs(i) = (s & 0xff).toChar
    cs(i + 1) = (s >> 8).toChar
    count = i + 2
  }

  override def write(s1: Short, s2: Short): Unit = {
    var cs = chars
    val i  = count
    if (i + 3 >= cs.length) {
      cs = Arrays.copyOf(cs, cs.length << 1)
      chars = cs
    }
    cs(i) = (s1 & 0xff).toChar
    cs(i + 1) = (s1 >> 8).toChar
    cs(i + 2) = (s2 & 0xff).toChar
    cs(i + 3) = (s2 >> 8).toChar
    count = i + 4
  }

  override def write(s1: Short, s2: Short, s3: Short): Unit = {
    var cs = chars
    val i  = count
    if (i + 5 >= cs.length) {
      cs = Arrays.copyOf(cs, cs.length << 1)
      chars = cs
    }
    cs(i) = (s1 & 0xff).toChar
    cs(i + 1) = (s1 >> 8).toChar
    cs(i + 2) = (s2 & 0xff).toChar
    cs(i + 3) = (s2 >> 8).toChar
    cs(i + 4) = (s3 & 0xff).toChar
    cs(i + 5) = (s3 >> 8).toChar
    count = i + 6
  }

  override def write(s1: Short, s2: Short, s3: Short, s4: Short): Unit = {
    var cs = chars
    val i  = count
    if (i + 7 >= cs.length) {
      cs = Arrays.copyOf(cs, cs.length << 1)
      chars = cs
    }
    cs(i) = (s1 & 0xff).toChar
    cs(i + 1) = (s1 >> 8).toChar
    cs(i + 2) = (s2 & 0xff).toChar
    cs(i + 3) = (s2 >> 8).toChar
    cs(i + 4) = (s3 & 0xff).toChar
    cs(i + 5) = (s3 >> 8).toChar
    cs(i + 6) = (s4 & 0xff).toChar
    cs(i + 7) = (s4 >> 8).toChar
    count = i + 8
  }

  def buffer: CharSequence = CharBuffer.wrap(chars, 0, count)

  override def toString: String = new String(chars, 0, count)
}
