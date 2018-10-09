(ns feedparser-clj.test.core
  (:require [clj-time.core :as t]
            [clojure.test :refer :all]
            [feedparser-clj.core :refer :all :reload true])
  (:import [com.rometools.rome.io SyndFeedInput XmlReader]
           java.io.InputStreamReader
           java.net.URL))

(defn load-feed-fixture [name]
  (str (clojure.java.io/resource (format "fixtures/%s" name))))

(deftest parse-test
  (let [pf (parse-feed (load-feed-fixture "gonzih-blog.xml"))]
    (testing :feed
      (is (= (-> pf :author) "gonzih@gmail.com (Max Gonzih)"))
      (is (= (-> pf :categories) []))
      (is (= (-> pf :contributors) []))
      (is (= (-> pf :entry-links) []))
      (is (= (-> pf :image) nil))
      (is (= (-> pf :copyright) "This work is licensed under a Creative Commons Attribution 4.0 International License."))
      (is (= (-> pf :description) "Recent content on Max Gonzih"))
      (is (= (-> pf :encoding) nil))
      (is (= (-> pf :feed-type) "rss_2.0"))
      (is (= (-> pf :language) "en-us"))
      (is (= (-> pf :link) "http://blog.gonzih.me/index.xml"))
      (is (= (-> pf :published-date) (t/date-time 2015 12 11)))
      (is (= (-> pf :title) "Max Gonzih"))
      (is (= (-> pf :uri) nil)))

    (testing :entry
      (is (= (-> pf :entries count) 15))
      (let [entry (-> pf :entries first)]
        (is (= (:authors entry) []))
        (is (= (:categories entry) []))
        (is (= (:contributors entry) []))
        (is (= (:enclosures entry) []))
        (is (= (:contents entry) []))
        (is (= "text/html" (:type (:description entry))))
        (is (re-find #"Collection of tweaks that I gathered after installing Arch.*" (:value (:description entry))))
        (is (= (:author entry) "gonzih@gmail.com (Max Gonzih)"))
        (is (= (:link entry) "http://blog.gonzih.me/blog/2015/12/11/arch-linux-on-lenovo-ideapad-y700-15/"))
        (is (= (:published-date entry) (t/date-time 2015 12 11)))
        (is (= (:title entry) "Arch Linux on Lenovo IdeaPad Y700 15\""))
        (is (= (:updated-date entry) nil))
        (is (= (:uri entry) "http://blog.gonzih.me/blog/2015/12/11/arch-linux-on-lenovo-ideapad-y700-15/"))))))
