.PHONY: default
default:
	elm make src/Main.elm --optimize --output ./elm.js

.PHONY: bruteforce
bruteforce: 
	elm make src/BruteForce.elm --optimize --output bruteforce.js && echo "this.Elm.BruteForce.init().ports.elmToJS.subscribe(x => console.log(x));" >>bruteforce.js && node bruteforce.js
