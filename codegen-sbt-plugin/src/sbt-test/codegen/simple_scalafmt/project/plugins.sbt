sys.props.get("plugin.version") match {
  case Some(x) => addSbtPlugin("io.github.ghostbuster91.sttp3-openapi3" % "sbt-codegen-plugin" % x)
  case _ => sys.error("""|The system property 'plugin.version' is not defined.
                         |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
}