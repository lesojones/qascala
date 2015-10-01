name := "Sbt101"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

//lazy val compileStyle = taskKey[Unit]("compileStyle")
//compileStyle := org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Compile).toTask("").value
//(compile in Compile) <<= (compile in Compile) dependsOn compileStyle
//
//(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"
//
//lazy val testStyle = taskKey[Unit]("testStyle")
//testStyle := org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Test).toTask("").value
//(test in Test) <<= (test in Test) dependsOn testStyle
