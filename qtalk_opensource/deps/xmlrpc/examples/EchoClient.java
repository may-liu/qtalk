import org.apache.xmlrpc.*;
import java.util.*;

public class EchoClient {
    public static void main(String[] args) {
	try {
	    XmlRpcClient xmlrpc = new XmlRpcClient("http://localhost:4567/");
	    Vector params = new Vector();
	    params.addElement(new Double(42.5));
	    params.addElement(new String("foo"));
	    params.addElement(new Integer(7));
	    System.out.println(xmlrpc.execute("echo", params));
	} catch (Exception e) {
	    System.err.println(e);
	}
    }
}
