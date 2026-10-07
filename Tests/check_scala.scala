/* Run through tools/isabelle --profile PROFILE -- scala mcp_library/Tests/check_scala.scala */
import scala.jdk.CollectionConverters._
import java.nio.file.Path
import isabelle.setup.{Build, Environment}

@main def check_library_compilation(): Unit = {
  // The runner starts with the complete application classpath; the compilations
  // below must not inherit it through the Scala JVM property or project jars.
  System.clearProperty("scala.usejavacp")
  val home = Path.of(Environment.getenv("ISABELLE_MCP_LIBRARY_HOME"))
  val project = home.getParent.toString + "/"
  val dependencies = Environment.getenv("ISABELLE_CLASSPATH").split(":")
    .filter(s => s.nonEmpty && !s.startsWith(project))
    .map(Path.of(_)).toList.asJava
  Build.build(dependencies, System.out, Build.component_context(home), true)
  Build.build(dependencies, System.out,
    Build.component_context(home.resolve("Tests/consumer")), true)
  println("Independent library and external-package consumer compilation passed (project jars excluded from ambient dependencies).")
}
