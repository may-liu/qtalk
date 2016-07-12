import org.apache.xmlrpc.*;
import java.util.*;

public class DateClient {
    public static void main(String[] args) {
	try {
	    XmlRpcClient xmlrpc = new XmlRpcClient("http://localhost:4567/");

	    // Login
	    Vector login_params = new Vector();
	    login_params.addElement(new String("Slarti"));
	    login_params.addElement(new String("Bartfast"));
	    System.out.println(xmlrpc.execute("login", login_params));

	    // Call 'days_since' function
	    Hashtable ymd = new Hashtable();
	    ymd.put("y", new Integer(2001));
	    ymd.put("m", new Integer(12));
	    ymd.put("d", new Integer(20));
	    Hashtable calc = new Hashtable();
	    calc.put("days_since", ymd);
	    Vector calc_params = new Vector();
	    calc_params.add(calc);
	    System.out.println(xmlrpc.execute("calc", calc_params));

	    // Call 'days_since', 'day_of_week' and 'is_leap_year' functions
	    calc.put("day_of_week", ymd);
	    calc.put("is_leap_year", new Integer(2000));
	    Vector calc_params2 = new Vector();
	    calc_params2.addElement(calc);
	    System.out.println(xmlrpc.execute("calc", calc_params2));

	    // Logout
	    System.out.println(xmlrpc.execute("logout", new Vector()));
	} catch (Exception e) {
	    System.err.println(e);
	}
    }
}
