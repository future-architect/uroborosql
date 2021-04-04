package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;
import java.util.Objects;
import java.util.Optional;

public class TestEntity {
	private long id;
	private String name;
	private int age;
	private LocalDate birthday;
	private Optional<String> memo;

	public TestEntity() {
	}

	public TestEntity(final long id, final String name, final int age, final LocalDate birthday,
			final Optional<String> memo) {
		this.id = id;
		this.name = name;
		this.age = age;
		this.birthday = birthday;
		this.memo = memo;
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

	public LocalDate getBirthday() {
		return this.birthday;
	}

	public Optional<String> getMemo() {
		return this.memo;
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

	public void setBirthday(final LocalDate birthday) {
		this.birthday = birthday;
	}

	public void setMemo(final Optional<String> memo) {
		this.memo = memo;
	}

	@Override
	public int hashCode() {
		return Objects.hash(age, birthday, id, memo, name);
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		var other = (TestEntity) obj;
		if (age != other.age) {
			return false;
		}
		if (!Objects.equals(birthday, other.birthday)) {
			return false;
		}
		if (id != other.id) {
			return false;
		}
		if (!Objects.equals(memo, other.memo)) {
			return false;
		}
		if (!Objects.equals(name, other.name)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TestEntity [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday + ", memo=" + memo
				+ "]";
	}
}
