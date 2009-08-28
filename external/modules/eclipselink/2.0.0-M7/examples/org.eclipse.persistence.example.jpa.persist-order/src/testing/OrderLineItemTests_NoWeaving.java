package testing;

public class OrderLineItemTests_NoWeaving extends OrderLineItemTests {

	@Override
	protected String getPUName() {
		return "persist-order-no-weaving";
	}

	@Override
	protected boolean requiresWeaving() {
		return false;
	}

}
