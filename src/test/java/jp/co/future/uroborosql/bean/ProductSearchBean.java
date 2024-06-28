package jp.co.future.uroborosql.bean;

import java.util.List;

public class ProductSearchBean extends BaseProductSearchBean {
	private List<Integer> productIds;

	public List<Integer> getProductIds() {
		return this.productIds;
	}

	public void setProductIds(final List<Integer> productIds) {
		this.productIds = productIds;
	}
}