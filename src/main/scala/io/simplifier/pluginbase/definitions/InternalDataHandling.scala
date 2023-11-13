package io.simplifier.pluginbase.definitions

object InternalDataHandling {

  case class PluginInformation(name: String,
                               version: String)


  case class Security(IncomingRequests: IncomingRequests)

  case class IncomingRequests(trustedSources: BlackAndWhiteList,
                              allowUntrustedSources: Boolean)


  case class BlackAndWhiteList(blackList: Seq[String],
                               whiteList: Seq[String])

}
