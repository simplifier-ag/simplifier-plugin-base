import lmcoursier.CoursierConfiguration
import sbt.*
import sbt.Keys.{credentials, csrConfiguration, resolvers, updateClassifiers}
import sbt.librarymanagement.Resolver
import sbt.librarymanagement.ivy.Credentials.toDirect

object SimplifierAzureRepo {

  val settings: Seq[Def.Setting[_]] = Seq(
    resolvers += resolver,
    credentials += SimplifierAzureRepo.userHomeCredentialsFile,
    csrConfiguration := SimplifierAzureRepo.fixAuthenticationByRepositoryId(credentials.value, csrConfiguration.value),
    updateClassifiers / csrConfiguration := csrConfiguration.value
  )

  val adoRepoHost = "pkgs.dev.azure.com"

  lazy val resolver: Resolver = "azure-maven" at "https://pkgs.dev.azure.com/simplifierag/_packaging/simplifierag/maven/v1"

  lazy val userHomeCredentialsFile = toDirect(Credentials(Path.userHome / ".sbt" / ".azure-credentials"))

  def fixAuthenticationByRepositoryId(credentials: Seq[Credentials], conf: CoursierConfiguration): CoursierConfiguration = {
    val adoCredentialsOpt = credentials.collectFirst { case creds: DirectCredentials if creds.host == adoRepoHost => creds }
    val newConfOpt = adoCredentialsOpt.map { adoCredentials =>
      val auths = conf.resolvers collect {
        case repo: MavenRepository if repo.root.startsWith(s"https://$adoRepoHost/") =>
          println(s"Adjust Azure repo $repo with $adoCredentials")
          repo.name -> lmcoursier.definitions.Authentication(adoCredentials.userName, adoCredentials.passwd)
      }
      auths.foldLeft(conf) { case (cc, (repoId, auth)) => cc.withAuthenticationByRepositoryId(Vector((repoId, auth))) }
    }
    newConfOpt.getOrElse(conf)
  }

}
