# Pragmatic Authentication Library: Cowboy example

The example using Pragmatic Authentication Library with Cowboy HTTP server.

### Overview

The project contains examples of authentication via:

- Google
- Facebook

To execute the examples on the your local machine you have to:

- Register an Google application at [Developers Console][google-developer-console].
- Register an Facebook application at [App Dashboard][facebook-app-dashboard].
- Specify the `client_id` and the `client_secret` in the `example.config` for each provider.

### Getting started

1. You need to install [rebar][rebar] to be able get and build project from code sources.

2. Getting dependencies:

			$ make deps

3. Starting an example application:

			$ make start

4. Open the URI in your browser:

	- Google: `https://localhost:8081/examples/oauth2/google`
	- Facebook: `https://localhost:8081/examples/oauth2/facebook`

See [pal][pal] project for more information.

### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[google-developer-console]:https://console.developers.google.com
[facebook-app-dashboard]:https://developers.facebook.com/apps
[rebar]:https://github.com/rebar/rebar
[pal]:https://github.com/manifest/pal

