libraryDependencies ++= Seq(
	compilerPlugin("org.scala-lang.plugins" % ("scala-continuations-plugin_" + scalaVersion.value) % "1.0.2"),
	"org.jogamp.jogl" % "jogl-all-main" % "2.3.1" ,
	"org.jogamp.jogl" % "jogl-all" % "2.3.1" ,
	"org.jogamp.gluegen" % "gluegen-rt-main" % "2.3.1" ,
	"org.jogamp.gluegen" % "gluegen-rt" % "2.3.1" ,
	"org.jogamp.jogl" % "nativewindow" % "2.3.1" ,
	"org.jogamp.jogl" % "newt" % "2.3.1"
)
	

scalaSource in Compile <<= baseDirectory(_ / "src")

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

autoCompilerPlugins := true

scalacOptions += "-P:continuations:enable"

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")
