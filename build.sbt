libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.12" % "test"
)

CrossBuilding.scriptedSettings

CrossBuilding.crossSbtVersions := Seq("0.12", "0.13")

libraryDependencies <+= CrossBuilding.sbtModuleDependencyInit("scripted-plugin")

crossBuildingSettings

CrossBuilding.latestCompatibleVersionMapper ~= { mapper => version =>
  version match {
    case "0.13" => "0.13.6"
    case x => mapper(x)
  }
}