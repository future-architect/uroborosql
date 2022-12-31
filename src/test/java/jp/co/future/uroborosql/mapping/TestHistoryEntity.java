package jp.co.future.uroborosql.mapping;

import java.time.LocalDate;
import java.util.Objects;

public class TestHistoryEntity {
	private Long id;
	private LocalDate startAt;
	private LocalDate finishAt;
	private String name;

	public TestHistoryEntity() {
	}

	public TestHistoryEntity(final Long id, final LocalDate startAt, final LocalDate finishAt, final String name) {
		this.id = id;
		this.startAt = startAt;
		this.finishAt = finishAt;
		this.name = name;
	}

	public Long getId() {
		return id;
	}

	public void setId(final Long id) {
		this.id = id;
	}

	public LocalDate getStartAt() {
		return startAt;
	}

	public void setStartAt(final LocalDate startAt) {
		this.startAt = startAt;
	}

	public LocalDate getFinishAt() {
		return finishAt;
	}

	public void setFinishAt(final LocalDate finishAt) {
		this.finishAt = finishAt;
	}

	public String getName() {
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	@Override
	public int hashCode() {
		return Objects.hash(finishAt, id, name, startAt);
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}
		if ((obj == null) || (getClass() != obj.getClass())) {
			return false;
		}
		var other = (TestHistoryEntity) obj;
		if (!Objects.equals(finishAt, other.finishAt)) {
			return false;
		}
		if (id != other.id) {
			return false;
		}
		if (!Objects.equals(name, other.name)) {
			return false;
		}
		if (!Objects.equals(startAt, other.startAt)) {
			return false;
		}
		return true;
	}

	@Override
	public String toString() {
		return "TestHistoryEntity [id=" + id + ", startAt=" + startAt + ", finishAt=" + finishAt + ", name=" + name
				+ "]";
	}

}
