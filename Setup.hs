import Distribution.PackageDescription (emptyHookedBuildInfo)
import Distribution.Simple
import Distribution.Simple.Command (noExtraFlags)
import System.Process


myUserHooks = simpleUserHooks {
                preBuild = fn
              }
    where fn args _ = do
            callProcess "/usr/bin/python" ["make_problem_module.py"]
            return emptyHookedBuildInfo

main = defaultMainWithHooks myUserHooks
