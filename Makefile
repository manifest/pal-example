PROJECT = pal_example

DEPS = pal_google_oauth2 pal_facebook_oauth2 cowboy
dep_pal_google_oauth2 = git git://github.com/manifest/pal-google-oauth2.git v0.2.1
dep_pal_facebook_oauth2 = git git://github.com/manifest/pal-facebook-oauth2.git v0.2.1
dep_cowboy = git git://github.com/extend/cowboy.git d2205d9ea6aa71ff256c48667755676d0e6c2377

PLT_APPS = pt pal jsx
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)' \
	-config $(CONFIG)

include erlang.mk
include ranch-ssl-patch.mk

deps::
	@sed -e "s/\(stdlib\$\)/\1,ssl/g" -i.back deps/ranch/src/ranch.app.src
