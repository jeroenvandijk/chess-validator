(ns user)

(comment
  (do
    (require '[nextjournal.clerk :as clerk])
    
    (System/setProperty "dev.env.interface" "nextjournal.clerk")
    
    (clerk/serve! {:watch-paths ["test" "src"]})    
    (clerk/show! "test/dev/jeroenvandijk/chess/validator/notebook_test.clj")
    
    :-)
  
  :-)
