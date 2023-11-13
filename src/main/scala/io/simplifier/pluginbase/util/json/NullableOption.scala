package io.simplifier.pluginbase.util.json

import org.json4s.Extraction.decompose
import org.json4s.{JValue, _}


/**
  * Extention to [[scala.Option]] which supports a third value for "value not known" ([[NUndefined]]).
  * The other values correspond like [[scala.None]] == [[NEmpty]] and [[scala.Some]] == [[NFull]].
  * There is also a JSON serializer to identify [[NEmpty]] with [[net.liftweb.json.JNull]]
  * and [[NUndefined]] with [[net.liftweb.json.JNothing]].
  *
  * @tparam A inner type of the nullable option
  */
sealed abstract class NullableOption[+A] {

  def isEmpty: Boolean

  def isUndefined: Boolean

  def isFull: Boolean

  def get: A

  def toString(default:Option[String]): String

  /**
    * Resolve to Option, by providing a fallback option which is used, if this is NUndefined.
    * NEmpty corresponds to None and NFull corresponds to Some.
    *
    * @param undefValue option value to use for undefined
    * @tparam B type super A, so it can be used in a contravarian position
    * @return resolve option
    */
  def toOption[B >: A](undefValue: Option[B]): Option[B] = this match {
    case NUndefined => undefValue
    case NEmpty => None
    case NFull(value) => Some(value)
  }

}

case object NEmpty extends NullableOption[Nothing] {

  override def isEmpty: Boolean = true

  override def isFull: Boolean = false

  override def isUndefined: Boolean = false

  override def get: Nothing = throw new UnsupportedOperationException("NEmpty.get")

  override def toString(default: Option[String]): String = default.getOrElse("")

  override def toString: String = ""
}

case object NUndefined extends NullableOption[Nothing] {

  override def isEmpty: Boolean = false

  override def isFull: Boolean = false

  override def isUndefined: Boolean = true

  override def get: Nothing = throw new UnsupportedOperationException("NUndefined.get")

  override def toString(default: Option[String]): String = default.getOrElse("")

  override def toString: String = ""
}

case class NFull[+A](value: A) extends NullableOption[A] {

  override def isEmpty: Boolean = false

  override def isFull: Boolean = true

  override def isUndefined: Boolean = false

  override def get: A = value

  override def toString(default: Option[String]): String = get.toString

  override def toString: String = get.toString
}

object NullableOption {

  val serializer: Serializer[NullableOption[_]] = new Serializer[NullableOption[_]] {

    private val C: Class[NullableOption[_]] = classOf[NullableOption[_]]

    override def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), NullableOption[_]] = {
      case (TypeInfo(C, ptype), json) => json match {
        case JNothing => NUndefined
        case JNull => NEmpty
        case other =>
          val tpe = ptype.getOrElse(throw new MappingException("parameterized type not known for NullableOption"))
          NFull(Extraction.extract(other, TypeInfo(tpe.getActualTypeArguments()(0).asInstanceOf[Class[_]], None)))
      }
    }

    override def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case NUndefined => JNothing
      case NEmpty => JNull
      case NFull(value) => decompose(value)
    }

  }

}
