package jp.co.future.uroborosql.model;

import jp.co.future.uroborosql.mapping.annotations.Table;

@Table(name = "EMP")
public class Emp {
	private String job;

	private Integer deptno;

	public String getJob() {
		return job;
	}

	public void setJob(final String job) {
		this.job = job;
	}

	public Integer getDeptno() {
		return deptno;
	}

	public void setDeptno(final Integer deptno) {
		this.deptno = deptno;
	}

}
