package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;
import java.util.Optional;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

public class TestDataNoKeyEntity {
	private long id;
	private String name;
	private int age;
	private LocalDate birthday;
	private Optional<String> memo;

	public TestDataNoKeyEntity() {
	}

	public TestDataNoKeyEntity(final long id, final String name, final int age, final LocalDate birthday, final Optional<String> memo) {
		this.id = id;
		this.name = name;
		this.age = age;
		this.birthday = birthday;
		this.memo = memo;
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
