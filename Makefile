
.PHONY: clean
clean:
	eldev clean all

.PHONY: lint
lint:
	eldev -C --unstable -T lint

