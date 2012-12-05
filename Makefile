
all: sendtest recvtest

sendtest:
	smbt sendtest

recvtest:
	smbt recvtest

clean:
	rm -f sendtest recvtest

.PHONY: sendtest recvtest clean

