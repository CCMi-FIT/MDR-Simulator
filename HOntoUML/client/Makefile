all: Main.hs Metamodel/*.hs Model/*.hs
	hastec Main.hs -o ../dist/main.js -Wall -fno-warn-missing-signatures --debug

production: Main.hs Metamodel.hs Model.hs
	make clean
	hastec Main.hs -o ../dist/main.js -Wall -fno-warn-missing-signatures --opt-all

clean:
	find -L . -name "*.o" -exec rm -rf {} \;
	find -L . -name "*.hi" -exec rm -rf {} \;
	find -L . -name "*.jsmod" -exec rm -rf {} \;
	find -L . -name "*.swp" -exec rm -rf {} \;
	rm -f ../dist/main.js
