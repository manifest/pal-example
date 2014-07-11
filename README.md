# Pragmatic Authentication Library: example

example of using Pragmatic Authentication Library in Cowboy web application

### Overview

The example project contains examples of authentication via:

- Google
- Facebook
- HTTP Basic Access Authentication

In case of executing Google and Facebook examples you need to:

- register an Google application at [Developers Console][google-developer-console]
- register an Facebook application at [App Dashboard][facebook-app-dashboard]
- setting up `client_id` and `client_secret` in the `example.config` for each provider

### Getting started

1. You need to install [rebar][rebar] for getting and building sources.

2. Get dependencies:

			$ make deps

3. Start an example application:

			$ make start

4. Open the URL of example in your browser:

	- Google: `https://localhost:8081/examples/oauth2/google`
	- Facebook: `https://localhost:8081/examples/oauth2/facebook`
	- HTTP Basic Access Authentication: `https://john:123@localhost:8081/examples/basic?pretty=true`

### Documentation

See [pal][pal] project for more information.

### License

Provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[google-developer-console]:https://console.developers.google.com
[facebook-app-dashboard]:https://developers.facebook.com/apps
[rebar]:https://github.com/rebar/rebar
[pal]:https://github.com/manifest/pal

