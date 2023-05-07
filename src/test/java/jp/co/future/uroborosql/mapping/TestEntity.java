package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;
import java.util.Objects;
import java.util.Optional;

public class TestEntity {
	private Long id;
	private String name;
	private Integer age;
	private LocalDate birthday;
	private Optional<String> memo;
	private Integer lockVersion;

	public TestEntity() {
	}

	public TestEntity(final Long id, final String name, final Integer age, final LocalDate birthday,
			final Optional<String> memo) {
		this.id = id;
		this.name = name;
		this.age = age;
		this.birthday = birthday;
		this.memo = memo;
		this.lockVersion = 0;
	}

	public Long getId() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public Integer getAge() {
		return this.age;
	}

	public LocalDate getBirthday() {
		return this.birthday;
	}

	public Optional<String> getMemo() {
		return this.memo;
	}

	public Integer getLockVersion() {
		return this.lockVersion;
	}

	public void setId(final Long id) {
		this.id = id;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public void setAge(final Integer age) {
		this.age = age;
	}

	public void setBirthday(final LocalDate birthday) {
		this.birthday = birthday;
	}

	public void setMemo(final Optional<String> memo) {
		this.memo = memo;
	}

	public void setLockVersion(final Integer lockVersion) {
		this.lockVersion = lockVersion;
	}

	@Override
	public int hashCode() {
		return Objects.hash(age, birthday, id, lockVersion, memo, name);
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		var other = (TestEntity) obj;
		if (!Objects.equals(age, other.age) || !Objects.equals(birthday, other.birthday)
				|| !Objects.equals(id, other.id) || !Objects.equals(lockVersion, other.lockVersion)) {
			return false;
		}
		if (!Objects.equals(memo, other.memo) || !Objects.equals(name, other.name)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TestEntity [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday + ", memo=" + memo
				+ ", lockVersion=" + lockVersion + "]";
	}

}
