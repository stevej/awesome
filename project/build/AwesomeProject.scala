import sbt._

class AsyncHttpProject(info: ProjectInfo) extends DefaultProject(info) {
  //val toolsSnapshot = "scala-tools-snapshot" at "http://scala-tools.org/repo-snapshots"
  // This is a magical value that I had to figure out by digging through the maven repo.
  val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5"
}
