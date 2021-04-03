package jp.co.future.uroborosql.mapping;

import java.util.Objects;

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
		return Objects.hash(id, key, name);
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
		var other = (TestDataMultiKeyEntity) obj;
		if (id != other.id) {
			return false;
		}
		if (!Objects.equals(key, other.key)) {
			return false;
		}
		if (!Objects.equals(name, other.name)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TestDataMultiKeyEntity [id=" + id + ", key=" + key + ", name=" + name + "]";
	}

}
