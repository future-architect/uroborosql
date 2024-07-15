package jp.co.future.uroborosql.model;

import java.util.Date;

import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

@Table(name = "PRODUCT")
public class Product {
	private Integer productId;
	private String productName;
	private String productKanaName;
	private String janCode;
	private String productDescription;
	private Date insDatetime;
	private Date updDatetime;
	@Version
	private Integer versionNo;

	public Product() {
	}

	public Product(final Integer productId,
			final String productName,
			final String productKanaName,
			final String janCode,
			final String productDescription,
			final Date insDatetime,
			final Date updDatetime,
			final Integer versionNo) {
		this.productId = productId;
		this.productName = productName;
		this.productKanaName = productKanaName;
		this.janCode = janCode;
		this.productDescription = productDescription;
		this.insDatetime = insDatetime;
		this.updDatetime = updDatetime;
		this.versionNo = versionNo;
	}

	public Integer getProductId() {
		return productId;
	}

	public void setProductId(final Integer productId) {
		this.productId = productId;
	}

	public String getProductName() {
		return productName;
	}

	public void setProductName(final String productName) {
		this.productName = productName;
	}

	public String getProductKanaName() {
		return productKanaName;
	}

	public void setProductKanaName(final String productKanaName) {
		this.productKanaName = productKanaName;
	}

	public String getJanCode() {
		return janCode;
	}

	public void setJanCode(final String janCode) {
		this.janCode = janCode;
	}

	public String getProductDescription() {
		return productDescription;
	}

	public void setProductDescription(final String productDescription) {
		this.productDescription = productDescription;
	}

	public Date getInsDatetime() {
		return insDatetime;
	}

	public void setInsDatetime(final Date insDatetime) {
		this.insDatetime = insDatetime;
	}

	public Date getUpdDatetime() {
		return updDatetime;
	}

	public void setUpdDatetime(final Date updDatetime) {
		this.updDatetime = updDatetime;
	}

	public Integer getVersionNo() {
		return versionNo;
	}

	public void setVersionNo(final Integer versionNo) {
		this.versionNo = versionNo;
	}
}