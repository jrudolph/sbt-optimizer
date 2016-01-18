sbtPlugin := true

CrossBuilding.scriptedSettings

CrossBuilding.crossSbtVersions := Seq("0.13")

libraryDependencies <+= CrossBuilding.sbtModuleDependencyInit("scripted-plugin")

crossBuildingSettings

CrossBuilding.latestCompatibleVersionMapper ~= { mapper => version =>
  version match {
    case "0.13" => "0.13.5"
    case x => mapper(x)
  }
}

ScalariformSupport.formatSettings

enablePlugins(net.virtualvoid.optimizer.SbtOptimizerPlugin)