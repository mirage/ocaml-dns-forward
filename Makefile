.PHONY: build clean test

build:
	jbuilder build --dev

test:
	jbuilder runtest --dev

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean
