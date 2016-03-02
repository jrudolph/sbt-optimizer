# sbt-optimizer - an sbt task execution analysis plugin

sbt is a powerful, yet complex build system. It allows declarative specification of tasks that
are later run in the right order so that dependencies of a task are run before the task itself.

In big projects, the tree of dependencies which is run to accomplish a single task can be quite
big and it can become unclear where sbt spends most of the time. In other cases, sbt tries to
run tasks as parallel as possible but it is unclear what are bottlenecks which prevent sbt from
finishing sooner.

sbt-optimizer is an experimental plugin that hooks into sbt's task execution engine and offers a graphical
ASCII report once a tree of tasks has been run:

## Usage

Add the plugin to `project/plugins.sbt`:

```
addSbtPlugin("net.virtual-void" % "sbt-optimizer" % "0.1.2")
```

and enable it in a project with

```
enablePlugins(net.virtualvoid.optimizer.SbtOptimizerPlugin)
```

After a task has been executed, the plugin will print statistics about the run as explained below.

## Explanation of the output

A line will be printed for every executed task that was running long enough to occupy a character
in the output line.

Each output line will look like this:

![Line](https://raw.githubusercontent.com/jrudolph/sbt-optimizer/master/docs/example-line.png)

Each output line corresponds to one task that has been executed. The first time is the total time this
task was running. The second time displayed in green is the actual execution time. The third time displayed in
red is time the task wanted to run but was waiting for the global ivy lock. The last time displayed in cyan
is the time the task was blocked waiting for Ivy downloads. The integer number displayed in cyan is the number
of downloads the task attempted. Then follows the abbreviated name of the task. Finally, a timeline is
displayed, that summarizes what the task did at which time:
 * a blank character means that the task was not registered in the task engine at that time
 * an orange '-' means the task was waiting for its dependencies
 * the green hash means that the task was actually running
 * a red 'L' means the task was waiting for the global ivy lock
 * a red '!' means the task was scheduled to run but no execution slot was available (as defined by sbt's `concurrentRestrictions`)

## Known issues

 * sbt allows to inject anonymous follow-up tasks using `Task.flatMap`. These get the special name
 `XYZ<called-by>` where XYZ was the original named task.
 * another source of tasks are tasks which are defined as `triggeredBy`

## License

The project is license under the Apache License v2. See the [LICENSE file](https://raw.githubusercontent.com/jrudolph/sbt-optimizer/master/LICENSE).
