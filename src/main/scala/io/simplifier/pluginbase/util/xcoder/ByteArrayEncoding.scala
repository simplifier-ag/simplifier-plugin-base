package io.simplifier.pluginbase.util.xcoder

import akka.util.ByteString
import io.simplifier.pluginbase.util.converter.StringUtils._
import io.simplifier.pluginbase.util.logging.Logging
import org.json4s.{JValue, _}

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

/**
  * Trait for Byte Array (Signed, Unsigned, Hex) Encoding and Decoding.
  *
  * @author Kamil Filar
  */
trait ByteArrayEncoding extends Logging {

  import io.simplifier.pluginbase.util.xcoder.ByteArrayEncoding._

  class Signed(array: Array[Byte]) {
    def value: Array[Byte] = array

    def toUnsigned: Try[Unsigned] = Try(new Unsigned(array.zipWithIndex.map { case (sb, i) => Try(sb & 0xff)
      .fold(e => throw new RuntimeException(s"Cannot decode signed byte: {$sb} on position: {$i} into a signed byte!", e), usb => usb)
    }))

    def toHex: Try[Hex] = Try(new Hex(array.zipWithIndex.map { case (sb, i) => Try(f"$sb%02X")
      .fold(e => throw new RuntimeException(s"Cannot decode signed byte: {$sb} on position: {$i} into a hexadecimal string!", e), hs => hs)
    }))

    def toByteString: ByteString = ByteString(array)


    override def toString: String = Arrays.toString(array)

    def toString(bracketOpen: String, bracketClose: String): String = Arrays.toString(array, bracketOpen, bracketClose)

    def toString(bracketOpen: String, separator: String, bracketClose: String): String = Arrays.toString(array, bracketOpen, separator, bracketClose)

    def toJson: JValue = JArray(value.map(signed => JInt(signed.toInt)).toList)

    def toJson(bracketOpen: String, bracketClose: String): JValue = JString(toString(bracketOpen, bracketClose))

    def toJson(bracketOpen: String, separator: String, bracketClose: String): JValue = JString(toString(bracketOpen, separator, bracketClose))
  }


  object Signed {
    /**
      * Implicit encoding method for the encoding an Signed-Byte-Array.
      *
      * @param value  the value to encode. It has to be an array or array string.
      * @tparam T     the return type. Here [[Signed]]
      *
      * @return       the Signed-Byte-Array wrapped in a Try-Monad
      */
    implicit def encode[T](value: T): Try[Signed] = {
      (value match {
        case sign: Signed => Success(sign.value)
        case unsign: Unsigned => Try(unsign.toSigned.get.value)
        case hex: Hex => hex.toSigned.map(_.value)
        case other@_ => Arrays.encodeArray[Byte](other, encodeValue, NoSignedArrayException, NoSignedArrayException)
      }).transform(sba => Success(new Signed(sba)),
        e => Failure(new RuntimeException(s"Error of the type: {${getExceptionTypeSafely(e)}} occurred during the encoding of a signed byte array!", e)))
    }

    /**
      * Encodes a single value into an signed byte integer.
      *
      * @param value  the value to encode.
      *
      * @return       the unsigned integer.
      */
    def encodeValue(value: Any): Byte = Arrays.encodeValue[Byte](value, encodeString, encodeByte, encodeInteger, NoValidValue, EncodingErrorHex[Byte])


    /**
      * Determines whether the current object is a valid Signed-Byte-Array and only a Signed-Byte-Array.
      *
      * @param `object`  the object to determine.
      * @tparam T        the type.
      *
      * @return          <b>true</b> if the provided object is a valid Signed-Byte-Array <b>false</b> otherwise.
      */
    def isArray[T](`object`: T): Boolean = {
      import Arrays._
      if (isEmptyArray(`object`)) true else {
        Arrays.isArray(`object`) && (`object` match {
          case _: Unsigned => true
          case _: ByteString => true
          case arr: Array[_] => arr.forall {
            case b: Byte => Try(criterionSigned(b.intValue)).getOrElse(false)
            case i: Int => Try(criterionSigned(i)).getOrElse(false)
            case l: Long => Try(criterionSigned(l.intValue)).getOrElse(false)
            case bi: BigInt => Try(criterionSigned(bi.intValue)).getOrElse(false)
            case f: Float => Try(f.isWhole() && criterionSigned(f.intValue)).getOrElse(false)
            case d: Double => Try(d.isWhole() && criterionSigned(d.intValue)).getOrElse(false)
            case bd: BigDecimal => Try(bd.isWhole() && criterionSigned(bd.intValue)).getOrElse(false)
            case s: String => Try(criterionSigned(Integer.parseInt(s))).getOrElse(false)
          }
          case s: String => isArrayString(s) && Try(extractFromArrayString(s).fold(_ => false, as => as.forall(str => criterionSigned(Integer.parseInt(str))))).getOrElse(false)
          case JString(s) => isArrayString(s) && Try(extractFromArrayString(s).fold(_ => false, as => as.forall(str => criterionSigned(Integer.parseInt(str))))).getOrElse(false)
          case JArray(values) => values.filterNot(_ == JNothing).forall {
            case JString(s) => Try(criterionSigned(Integer.parseInt(s))).getOrElse(false)
            case JInt(bi) => Try(criterionSigned(bi.intValue)).getOrElse(false)
            case JDouble(d) => Try(d.isWhole() && criterionSigned(d.intValue)).getOrElse(false)
            case _ => false
          }
          case _ => false
        })
      }
    }

    private[this] def encodeByte(byte: Byte): Byte = byte

    private[this] def encodeInteger(integer: Int): Byte = if (Arrays.criterionSigned(integer)) integer.toByte else throw new IllegalArgumentException(s"The integer: {$integer} is not a valid signed byte!")

    private[this] def encodeString(string: String): Byte = {
      if (Arrays.isHexString(string)) decodeString(string) else Try(encodeInteger(string.toInt)).orElse(Try(encodeByte(string.toByte)))
        .fold(e => throw new Exception(s"Cannot encode string: {$string} into a signed byte due to an error of the type: {${getExceptionTypeSafely(e)}}", e), bs => bs)
    }

    protected[ByteArrayEncoding] def decodeString(string: String): Byte = {
      Try(Integer.parseInt(Option(string.trim).getOrElse("")).toByte)
        .fold(e => throw new Exception(s"Cannot decode string: {$string} into a signed byte due to an error of the type: {${getExceptionTypeSafely(e)}}", e), bs => bs)
    }
  }


  class Unsigned(array: Array[Int]) {
    def value: Array[Int] = array

    def toSigned: Try[Signed] = Try(new Signed(array.zipWithIndex.map { case (usb, i) => Try(usb.toByte)
      .fold(e => throw new RuntimeException(s"Cannot decode unsigned byte: {$usb} on position: {$i} into a signed byte!", e), sb => sb)
    }))

    def toHex: Try[Hex] = Try(new Hex(array.zipWithIndex.map { case (usb, i) => Try(f"$usb%02X")
      .fold(e => throw new RuntimeException(s"Cannot decode unsigned byte: {$usb} on position: {$i} into a hexadecimal string!", e), hs => hs)
    }))

    def toByteString: Try[ByteString] = Try(ByteString(array.zipWithIndex.map { case (usb, i) => Try(usb.toByte)
      .fold(e => throw new RuntimeException(s"Cannot decode unsigned byte: {$usb} on position: {$i} into a signed byte!", e), sb => sb)
    }))

    override def toString: String = Arrays.toString(array)

    def toString(bracketOpen: String, bracketClose: String): String = Arrays.toString(array, bracketOpen, bracketClose)

    def toString(bracketOpen: String, separator: String, bracketClose: String): String = Arrays.toString(array, bracketOpen, separator, bracketClose)

    def toJson: JValue = JArray(value.map(unsigned => JInt(unsigned)).toList)

    def toJson(bracketOpen: String, bracketClose: String): JValue = JString(toString(bracketOpen, bracketClose))

    def toJson(bracketOpen: String, separator: String, bracketClose: String): JValue = JString(toString(bracketOpen, separator, bracketClose))
  }

  object Unsigned {

    /**
      * Implicit encoding method for the encoding an Unsigned-Byte-Array.
      *
      * @param value  the value to encode. It has to be an array or array string.
      * @tparam T     the return type. Here [[Unsigned]]
      *
      * @return       the Unsigned-Byte-Array wrapped in a Try-Monad
      */
    implicit def encode[T](value: T): Try[Unsigned] = {
      (value match {
        case sign: Signed => Try(sign.toUnsigned.get.value)
        case unsign: Unsigned => Success(unsign.value)
        case hex: Hex => hex.toUnsigned.map(_.value)
        case other@_ => Arrays.encodeArray[Int](other, encodeValue, NoUnsignedArrayException, NoUnsignedArrayException)
      }).transform(uba => Success(new Unsigned(uba)),
        e => Failure(new RuntimeException(s"Error of the type: {${getExceptionTypeSafely(e)}} occurred during the encoding of an unsigned byte array!", e)))
    }


    /**
      * Encodes a single value into an unsgined byte integer.
      *
      * @param value  the value to encode.
      *
      * @return       the unsigned integer.
      */
    def encodeValue(value: Any): Int = Arrays.encodeValue[Int](value, encodeString, encodeByte, encodeInteger, NoValidValue, EncodingErrorHex[Int])


    /**
      * Determines whether the current object is a valid Unsigned-Byte-Array and only a Unsigned-Byte-Array.
      *
      * @param `object`  the object to determine.
      * @tparam T        the type.
      *
      * @return          <b>true</b> if the provided object is a valid Unsigned-Byte-Array <b>false</b> otherwise.
      */
    def isArray[T](`object`: T): Boolean = {
      import Arrays._
      if (isEmptyArray(`object`)) true else {
        Arrays.isArray(`object`) && (`object` match {
          case _: Unsigned => true
          case bs: ByteString => bs.forall(b => criterionUnsigned(b.intValue))
          case arr: Array[_] => arr.forall {
            case b: Byte => Try(criterionUnsigned(b.intValue)).getOrElse(false)
            case i: Int => Try(criterionUnsigned(i)).getOrElse(false)
            case l: Long => Try(criterionUnsigned(l.intValue)).getOrElse(false)
            case bi: BigInt => Try(criterionUnsigned(bi.intValue)).getOrElse(false)
            case f: Float => Try(f.isWhole() && criterionUnsigned(f.intValue)).getOrElse(false)
            case d: Double => Try(d.isWhole() && criterionUnsigned(d.intValue)).getOrElse(false)
            case bd: BigDecimal => Try(bd.isWhole() && criterionUnsigned(bd.intValue)).getOrElse(false)
            case s: String => Try(criterionUnsigned(Integer.parseInt(s))).getOrElse(false)
          }
          case s: String => isArrayString(s) && Try(extractFromArrayString(s).fold(_ => false, as => as.forall(str => criterionUnsigned(Integer.parseInt(str))))).getOrElse(false)
          case JString(s) => isArrayString(s) && Try(extractFromArrayString(s).fold(_ => false, as => as.forall(str => criterionUnsigned(Integer.parseInt(str))))).getOrElse(false)
          case JArray(values) => values.filterNot(_ == JNothing).forall {
            case JString(s) => Try(criterionUnsigned(Integer.parseInt(s))).getOrElse(false)
            case JInt(bi) => Try(criterionUnsigned(bi.intValue)).getOrElse(false)
            case JDouble(d) => Try(d.isWhole() && criterionUnsigned(d.intValue)).getOrElse(false)
            case _ => false
          }
          case _ => false
        })
      }
    }


    private[this] def encodeByte(byte: Byte): Int = byte & 0xff

    private[this] def encodeInteger(integer: Int): Int = if (Arrays.criterionUnsigned(integer)) integer
    else throw new IllegalArgumentException(s"The integer: {$integer} is not a valid unsigned byte!")

    private[this] def encodeString(string: String): Int = {
      Try(decodeString(string)).recoverWith { case _ => Try(Hex.decodeString(string)) }
        .fold(e => throw new Exception(s"Cannot encode string: {$string} due to an error of the type: {${getExceptionTypeSafely(e)}}", e), hs => hs)
    }

    protected[ByteArrayEncoding] def decodeString(string: String): Int = {
      val sanitizedString: String = Option(string).filter(_.trim.nonEmpty).getOrElse("")
      Try(encodeInteger(Integer.parseInt(sanitizedString))).orElse(Try(encodeByte(Integer.parseInt(sanitizedString).toByte)))
        .fold(e => throw new Exception(s"Cannot decode string: {$string} into an unsigned byte due to an error of the type: {${getExceptionTypeSafely(e)}}", e), ubs => ubs)
    }
  }


  class Hex(array: Array[String]) {
    def value: Array[String] = array

    def toSigned: Try[Signed] = Try(new Signed(array.zipWithIndex.map { case (h, i) => Try(Hex.decodeString(h).toByte)
      .fold(e => throw new RuntimeException(s"Cannot decode hexadecimal string: {$h} on position: {$i} into a signed byte!", e), sb => sb)
    }))

    def toUnsigned: Try[Unsigned] = Try(new Unsigned(array.zipWithIndex.map { case (h, i) => Try(Hex.decodeString(h))
      .fold(e => throw new RuntimeException(s"Cannot decode hexadecimal string: {$h} on position: {$i} into an unsigned byte!", e), usb => usb)
    }))

    def toByteString: Try[ByteString] = Try(ByteString(array.zipWithIndex.map { case (h, i) => Try(Hex.decodeString(h).toByte)
      .fold(e => throw new RuntimeException(s"Cannot decode hexadecimal string: {$h} on position: {$i} into a signed byte!", e), sb => sb)
    }))

    override def toString: String = Arrays.toString(array)

    def toString(bracketOpen: String, bracketClose: String): String = Arrays.toString(array, bracketOpen, bracketClose)

    def toString(bracketOpen: String, separator: String, bracketClose: String): String = Arrays.toString(array, bracketOpen, separator, bracketClose)

    def toJson: JValue = JArray(value.map(hex => JString(hex)).toList)

    def toJson(bracketOpen: String, bracketClose: String): JValue = JString(toString(bracketOpen, bracketClose))

    def toJson(bracketOpen: String, separator: String, bracketClose: String): JValue = JString(toString(bracketOpen, separator, bracketClose))
  }


  object Hex {

    /**
      * Implicit encoding method for the encoding a Hex-Array.
      *
      * @param value  the value to encode. It has to be an array or array string.
      * @tparam T     the return type. Here [[Hex]]
      *
      * @return       the Hex-Array wrapped in a Try-Monad
      */
    implicit def encode[T](value: T): Try[Hex] = {
      (value match {
        case sign: Signed => Try(sign.toHex.get.value)
        case unsign: Unsigned => Try(unsign.toHex.get.value)
        case hex: Hex => Success(hex.value)
        case other@_ => Arrays.encodeArray(other, encodeValue, NoHexArrayException, NoHexArrayException)
      }).transform(ha => Success(new Hex(ha)),
        e => Failure(new RuntimeException(s"Error of the type: {${getExceptionTypeSafely(e)}} occurred during the encoding of a hex array!", e)))
    }

    /**
      * Encodes a single value into a hexadecimal string.
      *
      * @param value  the value to encode.
      *
      * @return       the hexadecimal string.
      */
    def encodeValue(value: Any): String = Arrays.encodeValue[String](value, encodeString, encodeByte, encodeInteger, NoValidValue, EncodingErrorHex[String])


    /**
      * Determines whether the current object is a valid Hex-Array and only a Hex-Array.
      *
      * @param `object`  the object to determine.
      * @tparam T        the type.
      *
      * @return          <b>true</b> if the provided object is a valid Hex-Array <b>false</b> otherwise.
      */
    def isArray[T](`object`: T): Boolean = {
      import Arrays._
      if (isEmptyArray(`object`)) true else {
        Arrays.isArray(`object`) && (`object` match {
          case _: Hex => true
          case arr: Array[_] => arr.forall {
            case s: String => isHexString(s)
            case _ => false
          }
          case str: String if isHexArrayString(str) => extractFromArrayString(str).fold(_ => false, as => as.forall(isHexString))
          case JString(s) if isHexArrayString(s) => extractFromArrayString(s).fold(_ => false, as => as.forall(isHexString))
          case JArray(values) => values.filterNot(_ == JNothing).forall {
            case JString(s) => isHexString(s)
            case _ => false
          }
          case _ => false
        })
      }
    }


    private[this] def encodeByte(byte: Byte): String = f"$byte%02X"

    private[this] def encodeInteger(int: Int): String = f"$int%02X"

    private[this] def encodeString(string: String): String = {
      if (Arrays.isHexString(string)) string else Try(encodeInteger(string.toInt)).orElse(Try(encodeByte(string.toByte)))
        .fold(e => throw new Exception(s"Cannot encode string: {$string} into a hexadecimal string due to an error of the type: {${getExceptionTypeSafely(e)}}", e), hs => hs)
    }

    protected[ByteArrayEncoding] def decodeString(string: String): Int = Try(Integer.parseInt(Option(string.trim).getOrElse(""), 16))
      .fold(e => throw new Exception(s"Cannot decode a hexadecimal string: {$string} into an unsigned byte due to an error of the type: {${getExceptionTypeSafely(e)}}", e), usb => usb)
  }


  object Arrays {
    def isArray[T](`object`: T): Boolean = {
      `object` match {
        case _: Array[_] => true
        case str: String if isArrayString(str) || isHexArrayString(str) || isByteString(str) => true
        case JString(s) if isArrayString(s) || isHexArrayString(s) || isByteString(s) => true
        case JArray(_) => true
        case _ => false
      }
    }


    def isEmptyArray[T](`object`: T): Boolean = {
      isArray(`object`) && (`object` match {
        case arr: Array[_] => arr.isEmpty
        case str: String if isArrayString(str) || isHexArrayString(str) => extractFromArrayString(str).fold(_ => false, as => as.isEmpty)
        case str: String if isByteString(str) => extractFromByteStringString(str).fold(_ => false, bsa => bsa.isEmpty)
        case JString(s) if isArrayString(s) || isHexArrayString(s) => extractFromArrayString(s).fold(_ => false, as => as.isEmpty)
        case JString(s) if isByteString(s) => extractFromByteStringString(s).fold(_ => false, bsa => bsa.isEmpty)
        case JArray(values) => Option(values.filterNot(_ == JNothing)).fold(true)(_.forall {
          case JInt(_) => false
          case JDouble(_) => false
          case JString(_) => false
          case _ => false
        })
        case _ => false
      })
    }


    def toString(array: Array[_]): String = array.mkString("[", ",", "]")

    def toString(array: Array[_], bracketOpen: String, bracketClose: String): String = toString(array, bracketOpen, ",", bracketClose)

    def toString(array: Array[_], bracketOpen: String, separator: String, bracketClose: String): String = {
      val sep: String = Option(separator).fold(",")(s => s)
      (Option(bracketOpen).filter(_.trim.nonEmpty), Option(bracketClose).filter(_.trim.nonEmpty)) match {
        case (None, None) => array.mkString(sep)
        case (ob, cb) => array.mkString(ob.getOrElse(""), sep, cb.getOrElse(""))
      }
    }


    private[ByteArrayEncoding] def isValidValue(`object`: Any): Boolean = {
      `object` match {
        case _: Byte => true
        case i: Int => criterionSigned(i) || criterionUnsigned(i)
        case l: Long => l.isValidInt && (criterionSigned(l.intValue()) || criterionUnsigned(l.intValue()))
        case bi: BigInt => bi.isValidInt && (criterionSigned(bi.intValue()) || criterionUnsigned(bi.intValue()))
        case JInt(i) => i.isValidInt && (criterionSigned(i.intValue()) || criterionUnsigned(i.intValue()))
        case f: Float => f.isWhole() && f.isValidInt && (criterionSigned(f.intValue()) || criterionUnsigned(f.intValue()))
        case d: Double => d.isWhole() && d.isValidInt && (criterionSigned(d.intValue()) || criterionUnsigned(d.intValue()))
        case bd: BigDecimal => bd.isWhole() && bd.isValidInt && (criterionSigned(bd.intValue()) || criterionUnsigned(bd.intValue()))
        case JDouble(d) => d.isValidInt && (criterionSigned(d.intValue()) || criterionUnsigned(d.intValue()))
        case s: String => isHexString(s) || Try(s.toByte).isSuccess || Try(s.toInt).map(i => criterionSigned(i) || criterionUnsigned(i)).isSuccess
        case JString(s) => isHexString(s) || Try(s.toByte).isSuccess || Try(s.toInt).map(i => criterionSigned(i) || criterionUnsigned(i)).isSuccess
        case _ => false
      }
    }


    private[ByteArrayEncoding] def encodeValue[T](value: Any, encoderString: String => T, encoderByte: Byte => T,
                                                  encoderInteger: Int => T, valueError: Any => Throwable, errorHandler: (Throwable, Any) => T): T = {
      Try(value match {
        case b: Byte => encoderByte(b)
        case i: Int if !Arrays.isValidValue(i) => throw valueError(i)
        case i: Int => encoderInteger(i)
        case l: Long if !Arrays.isValidValue(l) => throw valueError(l)
        case l: Long => encoderInteger(l.toInt)
        case bi: BigInt if !Arrays.isValidValue(bi) => throw valueError(bi)
        case bi: BigInt => encoderInteger(bi.intValue())
        case JInt(i) if !Arrays.isValidValue(i) => throw valueError(i)
        case JInt(i) => encoderInteger(i.intValue())
        case f: Float if !Arrays.isValidValue(f) => throw valueError(f)
        case f: Float => encoderInteger(f.toInt)
        case d: Double if !Arrays.isValidValue(d) => throw valueError(d)
        case d: Double => encoderInteger(d.toInt)
        case bd: BigDecimal if !Arrays.isValidValue(bd) => throw valueError(bd)
        case bd: BigDecimal => encoderInteger(bd.toInt)
        case JDouble(d) if !Arrays.isValidValue(d) => throw valueError(d)
        case JDouble(d) => encoderInteger(d.toInt)
        case s: String if !Arrays.isValidValue(s) => throw valueError(s)
        case s: String => encoderString(s)
        case JString(s) if !Arrays.isValidValue(s) => throw valueError(s)
        case JString(s) => encoderString(s)
      })
    }.fold(errorHandler(_, value), hs => hs)


    def encodeArray[T: ClassTag](value: Any, encoder: Any => T, noArrayException: Throwable, noArrayExceptionValue: Any => Throwable): Try[Array[T]] = {
      value match {
        case all@_ if !Arrays.isArray(all) => Failure(noArrayException)
        case all@_ if Arrays.isEmptyArray(all) => Success(Array.empty[T])
        case str: String if Arrays.isArrayString(str) || Arrays.isHexArrayString(str) => Arrays.extractFromArrayString(str).fold(e => Failure(ArrayStringExtractionException(e, str)),
          as => Try(as.zipWithIndex.map { case (v, i) => Try(encoder(v)).fold(errorHandler(_, v, i), s => s) }))
        case str: String if Arrays.isByteString(str) => Arrays.extractFromByteStringString(str).fold(e => Failure(ArrayStringExtractionException(e, str)),
          bsa => Try(bsa.zipWithIndex.map { case (v, i) => Try(encoder(v)).fold(errorHandler(_, v, i), s => s) }))
        case bs: ByteString => Try(bs.toArray.zipWithIndex.map { case (v, i) => Try(encoder(v)).fold(errorHandler(_, v, i), s => s) })
        case arr: Array[_] => Try(arr.zipWithIndex.map { case (v, i) => Try(encoder(v)).fold(errorHandler(_, v, i), s => s) })
        case JString(s) if Arrays.isArrayString(s) => Arrays.extractFromArrayString(s).fold(e => Failure(ArrayStringExtractionException(e, s)),
          as => Try(as.zipWithIndex.map { case (v, i) => Try(encoder(v)).fold(errorHandler(_, v, i), s => s) }))
        case JString(s) if Arrays.isByteString(s) => Arrays.extractFromByteStringString(s).fold(e => Failure(ArrayStringExtractionException(e, s)),
          bsa => Try(bsa.zipWithIndex.map { case (v, i) => Try(encoder(v)).fold(errorHandler(_, v, i), s => s) }))
        case JArray(values) => Try(values.zipWithIndex.map { case (v, i) => Try(encoder(v)).fold(errorHandler(_, v, i), s => s) }.toArray)
        case other@_ => Failure(noArrayExceptionValue(other))
      }
    }


    private[ByteArrayEncoding] def criterionSigned(int: Int): Boolean = int >= Byte.MinValue.toInt && int <= Byte.MaxValue.toInt

    private[ByteArrayEncoding] def criterionUnsigned(int: Int): Boolean = int >= 0 && int <= 255

    private[ByteArrayEncoding] def isArrayString(string: String): Boolean = REGEXP_STRING_ARRAY.findFirstMatchIn(string).nonEmpty

    private[ByteArrayEncoding] def isByteString(string: String): Boolean = REGEXP_BYTE_STRING.findFirstMatchIn(string).nonEmpty

    private[ByteArrayEncoding] def isHexArrayString(string: String): Boolean = REGEXP_HEX_ARRAY_STRING.findFirstMatchIn(string).nonEmpty

    private[ByteArrayEncoding] def isHexString(string: String): Boolean = REGEXP_HEX_STRING.findFirstMatchIn(string).nonEmpty


    private[ByteArrayEncoding] def extractFromArrayString(string: String): Try[Array[String]] = {
      Try(removeArrayBracketsAndQuotes(string).replaceAll(VALID_SEPARATORS_STRING_ARRAY.regex, ",").split(","))
    }

    private[ByteArrayEncoding] def extractFromByteStringString(string: String): Try[Array[String]] = {
      Try(extractByteString(string).replaceAll(VALID_SEPARATORS_BYTE_STRING.regex, ",").split(","))
    }
  }

}


object ByteArrayEncoding extends Logging {

  lazy val ByteArrayEncoder: ByteArrayEncoding = new ByteArrayEncoding {}


  private[ByteArrayEncoding] val VALID_SEPARATORS_STRING_ARRAY: Regex = "(?:\\s*,\\s*|\\s+)".r
  private[ByteArrayEncoding] val REGEXP_STRING_ARRAY: Regex = s"^(?:(?:(?:\\[?-?(?:[0-9]){1,3}${VALID_SEPARATORS_STRING_ARRAY.regex}))+-?(?:[0-9]){1,3}\\]?|\\[?-?(?:[0-9]){1,3}\\]?|\\[\\])$$".r
  private[ByteArrayEncoding] val VALID_SEPARATORS_BYTE_STRING: Regex = "[,]".r
  private[ByteArrayEncoding] val REGEXP_BYTE_STRING: Regex = s"^[B|b][Y|y][T|t][E|e][S|s][T|t][T|r][I|i][N|n][G|g](?:\\(?(?:-?\\s*[0-9]{1,3}\\s*${VALID_SEPARATORS_BYTE_STRING.regex}\\s*)+[0-9]{1,3}\\s*\\)?|\\(?\\s*\\-?[0-9]{1,3}\\s*\\)?|\\(\\s*\\))$$".r

  private[ByteArrayEncoding] val REGEXP_HEX_STRING: Regex = "^[0-9AaBbCcDdEeFf]{2}$".r
  private[ByteArrayEncoding] val REGEXP_HEX_STRING_INLINE: Regex = "[0-9AaBbCcDdEeFf]".r
  private[ByteArrayEncoding] val REGEXP_HEX_ARRAY_STRING: Regex = s"^(?:(?:(?:\\[?(?:${REGEXP_HEX_STRING_INLINE.regex}){2}${VALID_SEPARATORS_STRING_ARRAY.regex}))+(?:${REGEXP_HEX_STRING_INLINE.regex}){1,3}\\]?|\\[?(?:${REGEXP_HEX_STRING_INLINE.regex}){1,3}\\]?|\\[\\])$$".r


  private[ByteArrayEncoding] def errorHandler[T](error: Throwable, value: Any, position: Int): T = {
    throw new Exception(s"Error during encoding of value: {$value} on position: {$position} of type: {${getExceptionTypeSafely(error)}} ")
  }

  private[ByteArrayEncoding] final def NoValidValue(value: Any): IllegalArgumentException =
    new IllegalArgumentException(s"The provided value: {$value} is neither a hexadecimal, signed byte nor unsigned byte!")

  private final def EncodingErrorHex[T](error: Throwable, value: Any): T =
    throw new RuntimeException(s"Error of the type: {${getExceptionTypeSafely(error)}} during encoding of value: {$value} into an hex value!", error)

  private[ByteArrayEncoding] final def NoUnsignedArrayException = new IllegalArgumentException(s"The provided value is not convertible into an unsigned byte array!")

  private[ByteArrayEncoding] final def NoUnsignedArrayException(value: Any) = new IllegalArgumentException(s"The provided value: {${getClassNameSafely(value)}} is not an unsigned byte array!")

  private[ByteArrayEncoding] final def NoSignedArrayException = new IllegalArgumentException(s"The provided value is not convertible into a signed byte array!!")

  private[ByteArrayEncoding] final def NoSignedArrayException(value: Any) = new IllegalArgumentException(s"The provided value: {${getClassNameSafely(value)}} is not a signed byte array!")

  private[ByteArrayEncoding] final def NoHexArrayException = new IllegalArgumentException(s"The provided value is not convertible into a hex array!")

  private[ByteArrayEncoding] final def NoHexArrayException(value: Any) = new IllegalArgumentException(s"The provided value: {${getClassNameSafely(value)}} is not an convertible into a hex array!")

  private[ByteArrayEncoding] final def ArrayStringExtractionException(error: Throwable, value: String) = new Exception(s"The provided string: {$value} is cannot be extracted into a byte array string!", error)
}

