import Integration
import Snapshot
import Unit

main :: IO ()
main = do
  _ <- runUnitTests
  _ <- runIntegrationTests
  _ <- runSnapshotTests

  return ()