# purescript-book

Text and exercises for the PureScript book

## Update dependencies

Some of the dependencies listed originally in chapters' bower.json files are stale. It's best to:
1. Copy the deps on a side and remove them from bower.json
2. Run `rm -rf bower_components .pulp-cache`
3. (Optionally) create a new bower.json file with `pulp init`
4. `bower install -D -S purescript-psci-support`
5. `bower install -S dep1 dep2 dep3 ...`
6. If while installing the dependencies you're asked to pick specific versions, it's possible some of the packages are deprecated. Stop installing and install the packages one by one starting from the most basic ones. Then use Pursuit to find out which packages are deprecated and what they should be replaced with, as well as what changes in code it results in.
6. Validate the dependencies by running `pulp -w build`
7. Apply any code changes such as to the main:
```
- import Control.Monad.Eff (Eff)
- import Control.Monad.Eff.Console (CONSOLE, logShow)
+ import Effect (Effect)
+ import Effect.Console (logShow)

- main :: Eff (console :: CONSOLE) Unit
+ main :: Effect Unit
```
8. If it turns out the compiler needs to be upgraded to the latest version run `yarn global upgrade --latest purescript`

## Run the project

1. Running unit tests on watch: `pulp -w test -m "Test.Main"`
2. Running the build on watch: `pulp -w build`
3. Running the REPL: `pulp repl`
4. Building the dist: `pulp browserify --to dist/Main.js`
