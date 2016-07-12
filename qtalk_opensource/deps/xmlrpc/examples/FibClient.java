import org.apache.xmlrpc.*;
import java.util.*;

public class FibClient {
    public static void main(String[] args) {
	try {
	    if (args.length != 1) {
		System.err.println("Usage: FibClient N");
		System.exit(-1);
	    }

	    XmlRpcClient xmlrpc = new XmlRpcClient("http://localhost:4567/");
	    Vector params = new Vector();
	    params.addElement(new Integer(Integer.parseInt(args[0])));
	    System.out.println(xmlrpc.execute("fib", params));
	} catch (Exception e) {
	    System.err.println(e);
	}
    }
}
