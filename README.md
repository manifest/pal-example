# Pragmatic Authentication Library: Cowboy example

The example using Pragmatic Authentication Library with Cowboy HTTP server.

### Overview

The project contains examples of authentication via:

- Google
- Facebook

To execute the examples on the your local machine you need to:

- Register an Google application at [Developers Console][google-developer-console].
- Register an Facebook application at [App Dashboard][facebook-app-dashboard].
- Specify the `client_id` and the `client_secret` in the `rel/sys.config` for each provider.

### Getting started

1. Build and start the application:

			$ make && ./_rel/pal_example/bin/pal_example console

2. Open the URI in your browser:

	- Google: `https://localhost:8081/examples/oauth2/google`
	- Facebook: `https://localhost:8081/examples/oauth2/facebook`

See [pal][pal] project for more information.

### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[google-developer-console]:https://console.developers.google.com
[facebook-app-dashboard]:https://developers.facebook.com/apps
[pal]:https://github.com/manifest/pal

