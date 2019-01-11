package jp.co.future.uroborosql.mapping;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

import jp.co.future.uroborosql.mapping.annotations.Table;

@Table(name = "TEST")
public class TestEntityForUpdates {
	private long id;
	private String name;
	private int age;

	public TestEntityForUpdates() {
	}

	public TestEntityForUpdates(final long id, final String name, final int age) {
		this.id = id;
		this.name = name;
		this.age = age;
	}

	public long getId() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public int getAge() {
		return this.age;
	}


	public void setId(final long id) {
		this.id = id;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public void setAge(final int age) {
		this.age = age;
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
