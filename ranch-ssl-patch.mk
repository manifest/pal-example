.PHONY: ranch-ssl-patch

ranch-ssl-patch:
	sed -e "s/\(stdlib\$\)/\1,ssl/g" -i.back deps/ranch/src/ranch.app.src

