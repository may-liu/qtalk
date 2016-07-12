# Update the APACHE_XMLRPC_PATH variable to point at your Apache
# XML-RPC installation. This is only necessary if you intend to
# compile the Java clients.
APACHE_XMLRPC_PATH=../../apache/xmlrpc/bin/xmlrpc-1.1.jar

# Do not change anything below this line.
ERLC=erlc
ERLC_FLAGS=-W
MODULES=date_server echo_server fib_server validator
EBIN_FILES=$(MODULES:%=%.beam)
JAVAC=javac
JAVAC_FLAGS=-classpath $(APACHE_XMLRPC_PATH):.
JAVA_FILES=FibClient EchoClient DateClient BankClient
CLASS_FILES=$(JAVA_FILES:%=%.class)

all: $(EBIN_FILES) 

java_clients:  $(CLASS_FILES)

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<

%.class: %.java
	$(JAVAC) $(JAVAC_FLAGS) $<

clean:
	rm -f $(EBIN_FILES) $(CLASS_FILES)
