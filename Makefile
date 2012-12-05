
all: nanomq/include/nmq.hpp sendtest recvtest

sendtest: 
	smbt sendtest

recvtest:
	smbt recvtest

clean:
	rm -f sendtest recvtest

nanomq/include/nmq.hpp:
	git clone git://github.com/rigtorp/nanomq.git nanomq

.PHONY: sendtest recvtest clean

