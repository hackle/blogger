I've had the idea of re-building this blog in `Idris` for some times now. What do you know! AWS recently announced support for [custom runtimes](https://docs.aws.amazon.com/lambda/latest/dg/runtimes-custom.html#runtimes-custom-build), and what better time to start this?

Find the source code [here](https://github.com/hackle/idris-lambda-apigateway).


## set up

* create a Lambda function that uses custom runtime
* I am coding on `Ubuntu` and the compiled executables seem to be compatible with `AWS Linux` (which Lambda functions run on). Apparently this won't be the case for Windows or iOS so maybe developing through a container is required - I am keeping it simple here so I won't explore these options.

## `bootstrap`

The AWS documentation states that a `bootstrap` executable will act as the run time for a function's handler.

Starting simple is key to any success - so let's just have a bare minimum `bootstrap` that dumps all environment variables.

```idris
import System

printOne : (String, String) -> IO ()
printOne (a, b) = putStrLn $ a ++ ": " ++ b

main : IO ()
main = do
  envs <- System.getEnvironment
  sequence_ $ map printOne envs
```

Zip this and upload to Lambda, and I get a dump of ENVs!

```Javascript
START RequestId: 611d2694-fce9-11e8-86bf-2554391e3366 Version: $LATEST
PATH: /usr/local/bin:/usr/bin/:/bin:/opt/bin
LD_LIBRARY_PATH: /lib64:/usr/lib64:/var/runtime:/var/runtime/lib:/var/task:/var/task/lib:/opt/lib
LANG: en_US.UTF-8
TZ: :UTC
LAMBDA_TASK_ROOT: /var/task
LAMBDA_RUNTIME_DIR: /var/runtime
AWS_REGION: ap-southeast-2
AWS_DEFAULT_REGION: ap-southeast-2
AWS_LAMBDA_LOG_GROUP_NAME: /aws/lambda/blogIdris
AWS_LAMBDA_LOG_STREAM_NAME: 2018/12/11/[$LATEST]3968f189c6734e75864211ca960ab1fd
AWS_LAMBDA_FUNCTION_NAME: blogIdris
AWS_LAMBDA_FUNCTION_MEMORY_SIZE: 128
AWS_LAMBDA_FUNCTION_VERSION: $LATEST
_AWS_XRAY_DAEMON_ADDRESS: 169.254.79.2
_AWS_XRAY_DAEMON_PORT: 2000
AWS_XRAY_DAEMON_ADDRESS: 169.254.79.2:2000
AWS_XRAY_CONTEXT_MISSING: LOG_ERROR
_HANDLER: blog.handler
AWS_LAMBDA_RUNTIME_API: 127.0.0.1:9001
AWS_ACCESS_KEY_ID: ASIAUUEWB2VIWTDTII5Z
AWS_SECRET_ACCESS_KEY: ...scrubbed...
AWS_SESSION_TOKEN: ...scrubbed...
END RequestId: 611d2694-fce9-11e8-86bf-2554391e3366
REPORT RequestId: 611d2694-fce9-11e8-86bf-2554391e3366	Duration: 37.39 ms	Billed Duration: 100 ms 	Memory Size: 128 MB	Max Memory Used: 12 MB	
RequestId: 611d2694-fce9-11e8-86bf-2554391e3366 Error: Runtime exited without providing a reason
Runtime.ExitError
```

Not surprising that it ends in a error as it's not a complete runtime yet.