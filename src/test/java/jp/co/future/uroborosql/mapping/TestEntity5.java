package jp.co.future.uroborosql.mapping;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Objects;
import java.util.Optional;

import jp.co.future.uroborosql.mapping.annotations.Table;
import jp.co.future.uroborosql.mapping.annotations.Version;

@Table(name = "TEST")
public class TestEntity5 {
	private Long id;
	private String name;
	private Optional<BigDecimal> age;
	private LocalDate birthday;
	@Version
	private Integer lockVersion = 0;

	public TestEntity5() {
	}

	public TestEntity5(final Long id, final String name, final Optional<BigDecimal> age, final LocalDate birthday) {
		this.id = id;
		this.name = name;
		this.age = age;
		this.birthday = birthday;
	}

	public interface Names {
		String Id = "id";
		String Name = "name";
		String Age = "age";
		String Birthday = "birthday";
	}

	public interface Cols {
		String Id = "id";
		String Name = "name";
		String Age = "age";
		String Birthday = "birthday";
	}

	public Long getId() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public Optional<BigDecimal> getAge() {
		return this.age;
	}

	public LocalDate getBirthday() {
		return this.birthday;
	}

	public void setId(final Long id) {
		this.id = id;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public void setAge(final Optional<BigDecimal> age) {
		this.age = age;
	}

	public void setBirthday(final LocalDate birthday) {
		this.birthday = birthday;
	}

	public Integer getLockVersion() {
		return lockVersion;
	}

	public void setLockVersion(final Integer lockVersion) {
		this.lockVersion = lockVersion;
	}

	@Override
	public int hashCode() {
		return Objects.hash(age, birthday, id, lockVersion, name);
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
		var other = (TestEntity5) obj;
		if (!Objects.equals(age, other.age)) {
			return false;
		}
		if (!Objects.equals(birthday, other.birthday)) {
			return false;
		}
		if (!Objects.equals(id, other.id)) {
			return false;
		}
		if (!Objects.equals(lockVersion, other.lockVersion)) {
			return false;
		}
		if (!Objects.equals(name, other.name)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TestEntity5 [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday + ", lockVersion="
				+ lockVersion + "]";
	}

}