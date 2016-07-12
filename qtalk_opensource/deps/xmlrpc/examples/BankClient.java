import org.apache.xmlrpc.*;
import java.util.*;

public class BankClient {
    public static void main(String[] args) {
	if (args.length < 2) {
	    System.err.println("Usage: BankClient deposit Account Amount");
	    System.err.println("       BankClient withdraw Account Amount");
	    System.err.println("       BankClient balance Account");
	    System.exit(-1);
	    }

	if (!ask(3020, args))
	    if (!ask(3030, args))
		System.err.println("No bank server available");
    }

    private static boolean ask(int port, String[] args) {
	try {
	    // This is obviously not a unique tag.
	    String tag = new String(Long.toString(System.currentTimeMillis()));
	    XmlRpcClient xmlrpc =
		new XmlRpcClient("http://localhost:"+port+"/");
	    Vector params = new Vector();

	    params.addElement(tag);
	    params.addElement(new String(args[1]));

	    if (args[0].equals("deposit")) {
		params.addElement(new Integer(Integer.parseInt(args[2])));
		System.out.println(xmlrpc.execute("deposit", params));
		return true;
	    }
	    
	    if (args[0].equals("withdraw")) {
		params.addElement(new Integer(Integer.parseInt(args[2])));
		System.out.println(xmlrpc.execute("withdraw", params));
		return true;
	    }
	    
	    if (args[0].equals("balance")) {
		System.out.println(xmlrpc.execute("balance", params));
		return true;
	    }
	} catch (Exception e) {
	    System.err.println(e);
	}

	return false;
    }
}
