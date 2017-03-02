package jp.co.future.uroborosql.context.test.packtest;

public enum TestEnum2 {
	D, E, F;

	enum Inner {
		G, H, I {
		//インナークラス化
		};
	}
}
