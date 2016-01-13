package net.virtualvoid.optimizer

import sbt.{ Def, AutoPlugin }

object SbtOptimizerPlugin extends AutoPlugin {
  override def projectSettings: Seq[Def.Setting[_]] = IvyDownloadReporter.install()

  override def globalSettings: Seq[Def.Setting[_]] =
    Seq(
      IvyLockReporter.install(),
      ExecutionProgressReporter.install())
}