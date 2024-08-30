import play.core.PlayVersion
import play.sbt.PlayImport.*
import sbt.Keys.libraryDependencies
import sbt.*

object AppDependencies {

  private val playVersion = "play-30"
  private val bootstrapVersion = "9.0.0"
  private val hmrcMongoVersion = "2.1.0"

  val compile: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"             %% s"bootstrap-backend-$playVersion"  % bootstrapVersion,
    "uk.gov.hmrc.mongo"       %% s"hmrc-mongo-$playVersion"         % hmrcMongoVersion,
    "commons-io"              %  "commons-io"                       % "2.16.1"
  )

  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"             %% s"bootstrap-test-$playVersion"     % bootstrapVersion            % Test,
    "uk.gov.hmrc.mongo"       %% s"hmrc-mongo-test-$playVersion"    % hmrcMongoVersion            % Test,
    "org.mockito"             % "mockito-core"                      % "5.13.0"                    % Test
  )

  val it: Seq[ModuleID] = Seq(
    "org.mockito"             % "mockito-core"                      % "5.13.0"                    % Test
  )
}
