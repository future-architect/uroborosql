# 複数列IN句対応の改修計画

## 1. 概要

現在のUroboroSQLのDAOインタフェースでは、単一カラムに対するIN句のみサポートしていますが、複数カラムに対するIN句をサポートするための機能追加を行います。

## 2. 現状の問題点

現在提供されているIN句のAPIは単一カラムのみ対応：

```java
agent().query(Product.class)
    .in("productId", List.of(1, 2))
    .collect();
```

複数カラムのIN句には対応していないため、以下のようなSQLを発行できません：

```sql
SELECT * FROM product t
WHERE (t.product_id, t.product_name) IN ((1, '商品1'), (2, '商品2'))
```

## 3. 目標とする実装

### 3.1 新しいAPI

```java
var inParamBeans = List.of(
    new InParamBean(1, "商品1"), 
    new InParamBean(2, "商品2")
);

agent().query(Product.class)
    .in(inParamBeans)
    .collect();
```

※ `InParamBean`は`productId`と`productName`をフィールドに持つJavaBean

### 3.2 生成されるSQL

```sql
SELECT * FROM product t
WHERE (t.product_id, t.product_name) IN ((?, ?), (?, ?))
```

## 4. 修正対象ファイル

### 4.1 インタフェース定義の追加

**ファイル**: `src/main/java/jp/co/future/uroborosql/fluent/ExtractionCondition.java`

**変更内容**:
- 新しいメソッドシグネチャを追加：`<V> T in(Iterable<V> beans)`
- JavaDocコメントで、Beanの全フィールドが複数カラムのIN句として使用されることを明記

### 4.2 実装クラスの修正

**ファイル**: `src/main/java/jp/co/future/uroborosql/AbstractExtractionCondition.java`

**変更内容**:

1. **新しいOperatorクラスの追加**: `MultiColumnIn<V>`
   - Beanの集合を保持
   - TableMetadataへの参照を保持
   - Beanのフィールド情報とカラム名のマッピングを管理

2. **`in(Iterable<V> beans)`メソッドの実装**
   - MultiColumnInオペレータを生成
   - ExecutionContextにパラメータとして登録

3. **WHERE句生成ロジックの拡張**
   - `getWhereClause()`メソッドに、MultiColumnInの特別処理を追加
   - 新しいヘルパーメソッド`buildMultiColumnInClause()`を追加

4. **`buildMultiColumnInClause()`メソッドの実装**
   - Beanからフィールド値を抽出（BeanAccessor使用）
   - フィールド名をカラム名にマッピング（TableMetadata使用）
   - タプル形式のSQL文字列を生成：`(col1, col2) IN ((?, ?), (?, ?))`
   - 各値を個別のパラメータとしてExecutionContextに登録

### 4.3 テストクラスの追加

**ファイル**: `src/test/java/jp/co/future/uroborosql/SqlEntityQueryMultiColumnInTest.java`

**テストケース**:
1. 複数行のBeanリストでの検索
2. 空のリストでの検索（0件返却を期待）
3. 単一行のBeanリストでの検索
4. count()メソッドとの組み合わせ
5. first()メソッドとの組み合わせ
6. 他の条件（greaterEqualなど）との組み合わせ

## 5. 技術的詳細

### 5.1 Bean処理の仕組み

1. **リフレクションによるフィールド抽出**
   - `BeanAccessor.fields()`でBeanの全フィールドを取得
   - 各フィールドから値を抽出

2. **カラムマッピング**
   - フィールド名をCamelCaseに変換
   - `TableMetadata.getColumn()`でテーブルカラムと照合
   - マッチしたフィールドのみを使用

3. **パラメータバインディング**
   - 各値を個別のパラメータとして登録（例：`multiColumnIn_0_0`, `multiColumnIn_0_1`）
   - 2-way SQLのパラメータマーカー形式で埋め込み：`/*paramName*/''`

### 5.2 エッジケースの処理

1. **空のBeanリスト**
   - `1 = 0`条件を生成（常に偽）
   - 結果として0件が返される

2. **フィールドとカラムの不一致**
   - マッチしないフィールドは無視
   - 最低1つのフィールドがマッチする必要がある

3. **SQLインジェクション対策**
   - 全ての値はプリペアドステートメントのパラメータとしてバインド
   - ユーザ入力を直接SQL文字列に埋め込まない

## 6. 既存機能への影響

### 6.1 互換性

- 既存の単一カラムIN句（`in(String col, Iterable<V> valueList)`）との共存
- 新しいオーバーロードメソッドのため、既存コードへの影響なし

### 6.2 動作確認

- 既存の全テストが引き続きパスすることを確認
- SqlEntityQueryTestなど、既存のIN句を使用するテストに影響がないことを確認

## 7. 実装手順

1. **Phase 1: インタフェース定義**
   - `ExtractionCondition.java`に新しいメソッドシグネチャを追加

2. **Phase 2: Operatorクラス実装**
   - `AbstractExtractionCondition`に`MultiColumnIn`クラスを追加
   - 必要な内部ロジックを実装

3. **Phase 3: WHERE句生成ロジック**
   - `getWhereClause()`の拡張
   - `buildMultiColumnInClause()`の実装

4. **Phase 4: メソッド実装**
   - `in(Iterable<V> beans)`メソッドの実装

5. **Phase 5: テスト作成**
   - テストクラスとテストケースを作成
   - 全テストケースが成功することを確認

6. **Phase 6: 既存テスト検証**
   - 既存テストがすべてパスすることを確認
   - リグレッションテストの実施

7. **Phase 7: ビルド＆品質チェック**
   - ライセンスフォーマットの適用
   - コンパイル確認
   - パッケージビルド確認

## 8. リスク分析

### 8.1 潜在的なリスク

1. **パフォーマンス**
   - リフレクションの使用による軽微なオーバーヘッド
   - 影響：軽微（1回のみの処理）

2. **Bean構造の制約**
   - Beanのフィールドがテーブルカラムと一致する必要がある
   - 対策：明確なドキュメント化とエラーハンドリング

### 8.2 軽減策

- 包括的なテストケースによる動作保証
- 既存テストの継続実行によるリグレッション防止
- コードレビューによる品質確保

## 9. 完了条件

- [ ] 全ての新規テストがパス
- [ ] 全ての既存テストがパス（リグレッションなし）
- [ ] コンパイルエラーなし
- [ ] パッケージビルド成功
- [ ] ライセンスヘッダーフォーマット済み
- [ ] CodeQL セキュリティスキャンをパス
- [ ] コードレビュー完了

## 10. レビューポイント

1. API設計は適切か？
2. 実装アプローチは妥当か？
3. エッジケースは十分にカバーされているか？
4. パフォーマンスへの影響は許容範囲か？
5. ドキュメントは十分か？
