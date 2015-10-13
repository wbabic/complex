(require 'cljs.repl)
(require 'cljs.repl.node)

(cljs.repl/repl (cljs.repl.node/repl-env)
                :output-dir "target/cljs/node_dev/out")

(require 'complex.test-runner :reload-all)
(complex.test-runner/runner)
