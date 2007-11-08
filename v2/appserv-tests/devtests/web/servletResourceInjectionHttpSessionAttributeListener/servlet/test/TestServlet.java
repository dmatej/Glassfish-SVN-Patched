package test;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

public class TestServlet extends HttpServlet {

    public void doGet(HttpServletRequest req, HttpServletResponse res)
            throws IOException, ServletException {

        boolean passed = false;

        HttpSession session = req.getSession();

        // Causes configured HttpSessionAttributeListener to be invoked
        session.setAttribute("foo", new Object());

        // Presence of session attribute with name "success" indicates that
        // configured HttpSessionAttributeListener has passed all resource
        // injection tests
        Object success = session.getAttribute("success");
        if (success != null) {
            passed = true;
        }

        res.getWriter().print(passed);
    }
}
