package io.simplifier.pluginbase.interfaces

import com.github.swagger.akka.SwaggerHttpService
import com.github.swagger.akka.model.Info
import io.swagger.models.auth.BasicAuthDefinition
import io.swagger.models.{ExternalDocs, Scheme}

abstract class DocumentationInterfaceService extends SwaggerHttpService {
    override def schemes: List[Scheme] = List(Scheme.HTTP, Scheme.HTTPS)

    override val apiClasses: Set[Class[_]]
    val title: String
    val description: String
    val version: String = "1.0"
    val externalDocsDescription: String
    val externalDocsUrl: String = "https://community.simplifier.io/doc/current-release/"

    override val basePath: String = ""
    override val info: Info = Info(title = title, description = description, version = version)
    override val externalDocs: Option[ExternalDocs] = Some(new ExternalDocs(externalDocsDescription, externalDocsUrl))
    override val securitySchemeDefinitions: Map[String, BasicAuthDefinition] = Map("basicAuth" -> new BasicAuthDefinition())

  }