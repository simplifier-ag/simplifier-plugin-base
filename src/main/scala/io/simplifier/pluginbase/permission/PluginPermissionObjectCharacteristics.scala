package io.simplifier.pluginbase.permission

/**
  * Characteristics for plugin permissions.
  */
object PluginPermissionObjectCharacteristics {

  /**
    * Abstract  permission characteristic class.
    */
  sealed trait CharacteristicDefinition

  /**
    * Boolean characteristic, rendered as CheckBox.
    */
  case class CheckboxCharacteristic(technicalName: String, name: String, description: String, defaultValue: Boolean = true) extends CharacteristicDefinition

  /**
    * Enum characteristic, rendered as DropDown.
    */
  case class DropDownCharacteristic(technicalName: String, name: String, description: String, values: Seq[String], defaultValue: String) extends CharacteristicDefinition

  /**
    * FreeText characteristic, rendered as TextField.
    */
  case class TextFieldCharacteristic(technicalName: String, name: String, description: String, defaultValue: String = "") extends CharacteristicDefinition

  /**
    * FreeText characteristic, rendered as TextField.
    */
  case class MultiSelectCharacteristic(technicalName: String, name: String, description: String, values: Set[String], defaultValue: String = "") extends CharacteristicDefinition

}
