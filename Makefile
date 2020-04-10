.PHONY: default
default:
	elm make src/Main.elm --optimize --output ./elm.js

.PHONY: bruteforce
bruteforce: 
	elm make src/BruteForce.elm --output bruteforce.js && echo "this.Elm.BruteForce.init();" >>bruteforce.js && node bruteforce.js
