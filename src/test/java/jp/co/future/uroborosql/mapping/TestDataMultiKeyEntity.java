package jp.co.future.uroborosql.mapping;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

@SuppressWarnings("unused")
public class TestDataMultiKeyEntity {
	private long id;
	private String key;
	private String name;

	public TestDataMultiKeyEntity() {
	}

	public TestDataMultiKeyEntity(final long id, final String key, final String name) {
		this.id = id;
		this.key = key;
		this.name = name;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this, true);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj, true);
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}
