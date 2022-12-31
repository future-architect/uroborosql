package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;
import java.util.Objects;
import java.util.Optional;

import jp.co.future.uroborosql.enums.GenerationType;
import jp.co.future.uroborosql.mapping.annotations.GeneratedValue;
import jp.co.future.uroborosql.mapping.annotations.Id;
import jp.co.future.uroborosql.mapping.annotations.Table;

@Table(name = "TEST")
public class TestEntityWithDefaultValue {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	private Optional<String> name;
	private Optional<Integer> age;
	private Optional<LocalDate> birthday;
	private Optional<String> memo;

	public TestEntityWithDefaultValue() {
	}

	public TestEntityWithDefaultValue(final Long id, final Optional<String> name, final Optional<Integer> age,
			final Optional<LocalDate> birthday, final Optional<String> memo) {
		this.id = id;
		this.name = name;
		this.age = age;
		this.birthday = birthday;
		this.memo = memo;
	}

	public Long getId() {
		return id;
	}

	public void setId(final Long id) {
		this.id = id;
	}

	public Optional<String> getName() {
		return name;
	}

	public void setName(final Optional<String> name) {
		this.name = name;
	}

	public Optional<Integer> getAge() {
		return age;
	}

	public void setAge(final Optional<Integer> age) {
		this.age = age;
	}

	public Optional<LocalDate> getBirthday() {
		return birthday;
	}

	public void setBirthday(final Optional<LocalDate> birthday) {
		this.birthday = birthday;
	}

	public Optional<String> getMemo() {
		return memo;
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
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		var other = (TestEntityWithDefaultValue) obj;
		if (!Objects.equals(age, other.age) || !Objects.equals(birthday, other.birthday) || !Objects.equals(id, other.id) || !Objects.equals(memo, other.memo)) {
			return false;
		}
		if (!Objects.equals(name, other.name)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TestEntityWithDefaultValue [id=" + id + ", name=" + name + ", age=" + age + ", birthday=" + birthday
				+ ", memo=" + memo + "]";
	}

}
