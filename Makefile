.PHONY: default
default:
	elm make src/Main.elm --optimize --output ./elm.js
