# Micrometer統合計画書

## 概要

UroboroSQLでMicrometerを使用してSQL発行やSQL処理時間をメトリクスとして収集できるようにする改修計画です。

## 目的

- SQL実行回数の計測
- SQL実行時間の計測  
- SQLの種類（SELECT/UPDATE/INSERT/DELETE等）別のメトリクス収集
- Micrometerを通じて様々なモニタリングシステム（Prometheus、Datadog、CloudWatch等）へのメトリクス送信を可能にする

## 設計方針

### 1. アーキテクチャ

UroboroSQLは既存のイベントサブスクライバー機構を持っているため、この仕組みを活用します：

- **EventSubscriber**: UroboroSQLの既存イベント機構
- **AfterSqlQueryEvent/AfterSqlUpdateEvent/AfterSqlBatchEvent等**: SQL実行後のイベント
- **ExecutionContext**: SQL実行コンテキスト（SQL名、SQLの種類等の情報を保持）

### 2. 実装アプローチ

#### 2.1 依存関係の追加

`pom.xml`にMicrometerの依存を追加（optional）：
- `io.micrometer:micrometer-core` - Micrometerのコアライブラリ

#### 2.2 MicrometerEventSubscriberの実装

新しいイベントサブスクライバー `MicrometerEventSubscriber` を作成：

**場所**: `src/main/java/jp/co/future/uroborosql/event/subscriber/MicrometerEventSubscriber.java`

**主な機能**:
- `MeterRegistry`を保持（Micrometerのメトリクス登録先）
- 以下のイベントリスナーを実装：
  - `afterSqlQueryListener` - SELECT文実行後
  - `afterSqlUpdateListener` - UPDATE/INSERT/DELETE文実行後
  - `afterSqlBatchListener` - バッチ実行後
  - `afterProcedureListener` - ストアドプロシージャ実行後

**収集するメトリクス**:

1. **実行回数カウンター** (`Counter`)
   - メトリクス名: `uroborosql.sql.executions`
   - タグ:
     - `sql.type`: SQL種別（query, update, batch, procedure）
     - `sql.name`: SQL名（オプション、設定可能）
     - `sql.id`: SQL-ID（オプション、設定可能）

2. **実行時間タイマー** (`Timer`)
   - メトリクス名: `uroborosql.sql.duration`
   - タグ:
     - `sql.type`: SQL種別（query, update, batch, procedure）
     - `sql.name`: SQL名（オプション、設定可能）
     - `sql.id`: SQL-ID（オプション、設定可能）
   - 統計情報: 合計時間、カウント、最大値、パーセンタイル等

3. **処理行数ゲージ/サマリー** (`DistributionSummary`)
   - メトリクス名: `uroborosql.sql.rows`
   - タグ:
     - `sql.type`: SQL種別（query, update, batch, procedure）
     - `sql.name`: SQL名（オプション、設定可能）
     - `sql.id`: SQL-ID（オプション、設定可能）

#### 2.3 実行時間の計測

既存のイベント機構では実行時間が直接提供されていないため、以下の対応が必要：

**オプション1: ExecutionContextに実行時間を記録**
- `ExecutionContext`に開始時刻と終了時刻を記録するフィールドを追加
- SQL実行前後で時刻を記録
- イベントから`ExecutionContext`経由で実行時間を取得

**オプション2: イベントに実行時間を追加**
- `AfterSqlQueryEvent`等のイベントクラスに実行時間フィールドを追加
- イベント発行時に実行時間を設定

**推奨**: オプション1（既存のExecutionContextを拡張）
- 既存のイベントクラスの変更を最小限に抑える
- ExecutionContextは既に各種情報を保持する設計になっている

#### 2.4 設定オプション

`MicrometerEventSubscriber`のコンストラクタまたはセッターで以下を設定可能に：

- `meterRegistry`: 使用するMeterRegistry（必須）
- `includeQueryExecutionTime`: クエリ実行時間を計測するか（デフォルト: true）
- `includeSqlNameTag`: SQL名をタグに含めるか（デフォルト: false、カーディナリティ対策）
- `includeSqlIdTag`: SQL-IDをタグに含めるか（デフォルト: false、カーディナリティ対策）
- `includeRowCount`: 処理行数を計測するか（デフォルト: true）

## 実装計画

### Phase 1: 基本実装

1. ✅ 現状調査とアーキテクチャ理解
2. `pom.xml`にMicrometer依存を追加
3. `ExecutionContext`に実行時間計測機能を追加
   - 開始時刻フィールドの追加
   - 実行時間取得メソッドの追加
4. SQL実行箇所で開始時刻の記録を追加
5. `MicrometerEventSubscriber`の実装
   - 基本的なカウンターとタイマーの実装
   - SQL種別ごとのメトリクス記録
6. ライセンスヘッダーの追加（`mvn license:format`）

### Phase 2: テスト実装

7. `MicrometerEventSubscriberTest`の作成
   - MeterRegistryのモック/SimpleMeterRegistryを使用
   - 各SQL種別でメトリクスが正しく記録されることを確認
   - タグが正しく設定されることを確認
   - 実行時間が計測されることを確認
8. 統合テスト
   - 既存のテストが壊れていないことを確認

### Phase 3: ドキュメント整備

9. README.mdまたは別ドキュメントに使用例を追加
10. Javadocの整備

## 使用例

```java
// Micrometer MeterRegistryの作成（例：Prometheus）
MeterRegistry registry = new PrometheusMeterRegistry(PrometheusConfig.DEFAULT);

// UroboroSQL設定
SqlConfig config = UroboroSQL.builder("jdbc:h2:mem:test", "sa", "")
    .build();

// MicrometerEventSubscriberを追加
MicrometerEventSubscriber micrometerSubscriber = new MicrometerEventSubscriber(registry)
    .setIncludeSqlNameTag(true)  // SQL名をタグに含める（オプション）
    .setIncludeSqlIdTag(false);   // SQL-IDはタグに含めない（デフォルト）

config.getEventListenerHolder().addEventSubscriber(micrometerSubscriber);

// SQL実行
try (SqlAgent agent = config.agent()) {
    agent.query("example/select_product")
        .param("product_id", 1)
        .collect();
}

// メトリクスの確認
// registry.counter("uroborosql.sql.executions", "sql.type", "query").count()
// registry.timer("uroborosql.sql.duration", "sql.type", "query").mean()
```

## セキュリティ・パフォーマンス考慮事項

### カーディナリティ問題
- デフォルトではSQL名やSQL-IDをタグに含めない
- これらのタグは動的に増加する可能性があるため、明示的に有効化が必要
- 代わりに`sql.type`という限定的なタグのみをデフォルトで使用

### パフォーマンス影響
- メトリクス記録は非同期または軽量な操作のみ
- Micrometerのライブラリはoptional依存として、使用しない場合は影響なし
- イベントリスナーの追加/削除は動的に可能

### エラーハンドリング
- メトリクス記録の失敗がSQL実行に影響を与えないよう、try-catchで保護

## 技術的な詳細

### Micrometerとは
- Java用の計測ファサードライブラリ
- SLF4Jのメトリクス版
- 様々なモニタリングシステムへの統一的なインターフェース提供
- Spring Boot Actuatorでも採用されている標準的なライブラリ

### サポートされるメトリクスシステム
- Prometheus
- Datadog
- New Relic
- CloudWatch
- Graphite
- InfluxDB
- その他多数

### メトリクスの種類
- **Counter**: 増加のみ可能なカウンター（実行回数等）
- **Timer**: 実行時間の計測と統計
- **Gauge**: 現在の値（接続数等、今回は使用しない見込み）
- **DistributionSummary**: 分布の統計（処理行数等）

## リスク・制約事項

1. **既存コードへの影響**
   - ExecutionContextのインターフェース変更が必要
   - 後方互換性に注意

2. **依存ライブラリの追加**
   - Micrometerをoptional依存として追加
   - ライブラリサイズとライセンスの確認が必要

3. **テストの複雑性**
   - 時間計測のテストは環境依存の可能性
   - モックやテスト用のMeterRegistryを使用

## 代替案の検討

### 代替案1: Spring Boot Actuatorのみサポート
- **メリット**: Spring環境での統合が簡単
- **デメリット**: Spring以外の環境で使用できない

### 代替案2: 独自のメトリクスインターフェース
- **メリット**: 依存ライブラリなし
- **デメリット**: 標準的でない、既存のツールとの統合が困難

### 採用案: Micrometer直接サポート（推奨）
- **メリット**: 
  - 業界標準
  - 多様なバックエンドサポート
  - Spring Bootとも統合可能
- **デメリット**: 
  - 新しい依存ライブラリの追加

## まとめ

既存のEventSubscriber機構を活用し、Micrometerを統合することで、最小限の変更でメトリクス収集機能を追加できます。optional依存とすることで、既存のユーザーには影響を与えず、必要なユーザーのみが利用できる設計とします。
