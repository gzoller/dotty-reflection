resolvers += Resolver.url(
  "co.blocke ivy resolver",
  url("https://dl.bintray.com/blocke/releases/")
)(Resolver.ivyStylePatterns)
addSbtPlugin("co.blocke" % "gitflow-packager" % "0.1.8")
addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")
addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.3.4")