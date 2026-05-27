test-while-win:
	./compiler.exe tests/while_test.gex test.s
	diff test.s tests/while_test.expected.s
	@echo "PASS"

test-while-mac:
	./compiler.dmg tests/while_test.gex test.s
	diff test.s tests/while_test.expected.s
	@echo "PASS"