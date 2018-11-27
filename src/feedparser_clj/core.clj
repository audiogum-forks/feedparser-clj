(ns feedparser-clj.core
  (:import [java.net URL]
           [java.io InputStreamReader]
           [com.rometools.rome.io SyndFeedInput XmlReader])
  (:require [clj-time.coerce :as c]))

(defrecord feed [authors author categories contributors copyright description
                 encoding entries feed-type image language link entry-links
                 published-date title uri block closed-captioned keywords
                 season summary subtitle episode-type order episode duration])

(defrecord entry [authors author categories contents contributors description
                  enclosures link published-date title updated-date url uri block
                  explicit subtitle keywords summary type complete itunes-categories])

(defrecord enclosure [length type uri])
(defrecord person    [email name uri])
(defrecord category  [name taxonomy-uri])
(defrecord content   [type value])
(defrecord image     [description link title url])
(defrecord link      [href hreflang length rel title type])

(defn- obj->enclosure
  "Create enclosure struct from SyndEnclosure"
  [e]
  (map->enclosure {:length (.getLength e)
                   :type   (.getType e)
                   :url    (.getUrl e)}))

(defn- obj->content
  "Create content struct from SyndContent"
  [c]
  (map->content {:type  (.getType c)
                 :value (.getValue c)}))

(defn- obj->link
  "Create link struct from SyndLink"
  [l]
  (map->link {:href     (.getHref l)
              :hreflang (.getHreflang l)
              :length   (.getLength l)
              :rel      (.getRel l)
              :title    (.getTitle l)
              :type     (.getType l)}))

(defn- obj->category
  "Create category struct from SyndCategory"
  [c]
  (map->category {:name         (.getName c)
                  :taxonomy-uri (.getTaxonomyUri c)}))

(defn- obj->person
  "Create a person struct from SyndPerson"
  [sp]
  (map->person {:email (.getEmail sp)
                :name  (.getName sp)
                :uri   (.getUri sp)}))

(defn- obj->image
  "Create image struct from SyndImage"
  [i]
  (map->image {:description (.getDescription i)
               :link        (.getLink i)
               :title       (.getTitle i)
               :url         (.getUrl i)}))

(defn- obj->entry
  "Create feed entry struct from SyndEntry"
  [e]
  (let [itunes-info (.getModule e "http://www.itunes.com/dtds/podcast-1.0.dtd")]
    (map->entry {:authors          (map obj->person    (seq (.getAuthors e)))
                 :categories       (map obj->category  (seq (.getCategories e)))
                 :contents         (map obj->content   (seq (.getContents e)))
                 :contributors     (map obj->person    (seq (.getContributors e)))
                 :enclosures       (map obj->enclosure (seq (.getEnclosures e)))
                 :description      (if-let [d (.getDescription e)] (obj->content d))
                 :author           (or (some-> itunes-info .getAuthor) (.getAuthor e))
                 :link             (.getLink e)
                 :published-date   (c/from-date (.getPublishedDate e))
                 :title            (or (some-> itunes-info .getTitle) (.getTitle e))
                 :subtitle         (some-> itunes-info .getSubtitle)
                 :updated-date     (c/from-date (.getUpdatedDate e))
                 :uri              (.getUri e)
                 :image            (if-let [i (some-> itunes-info .getImage str)]
                                     {:url i})
                 :block            (some-> itunes-info .getBlock)
                 :closed-captioned (some-> itunes-info .getClosedCaptioned)
                 :keywords         (some-> itunes-info .getKeywords seq)
                 :summary          (some-> itunes-info .getSummary)
                 :season           (some-> itunes-info .getSeason)
                 :episode-type     (some-> itunes-info .getEpisodeType)
                 :order            (some-> itunes-info .getOrder)
                 :episode          (some-> itunes-info .getEpisode)
                 :duration         (some-> itunes-info .getDuration .getMilliseconds (quot 1000))})))

(defn- categories
  "Return a vector of categories, each category is a vector potentially
  of [cat sub-cat] pairs."
  [c]
  (if-let [subcategories (seq (.getSubcategories c))]
    (mapv (fn [s] [(.getName c) (.getName s)]) subcategories)
    [[(.getName c)]]))

(defn- obj->feed
  "Create a feed struct from a SyndFeed"
  [f]
  (let [itunes-info (.getModule f "http://www.itunes.com/dtds/podcast-1.0.dtd")]
    (map->feed  {:authors           (map obj->person   (seq (.getAuthors f)))
                 :categories        (map obj->category (seq (.getCategories f)))
                 :contributors      (map obj->person   (seq (.getContributors f)))
                 :entries           (map obj->entry    (seq (.getEntries f)))
                 :entry-links       (map obj->link     (seq (.getLinks f)))
                 :image             (if-let [itunes-image (some-> itunes-info .getImage)]
                                      {:url (str itunes-image)}
                                      (some-> f .getImage obj->image))
                 :author            (or (some-> itunes-info .getAuthor) (.getAuthor f))
                 :copyright         (.getCopyright f)
                 :description       (.getDescription f)
                 :encoding          (.getEncoding f)
                 :feed-type         (.getFeedType f)
                 :language          (.getLanguage f)
                 :link              (.getLink f)
                 :published-date    (c/from-date (.getPublishedDate f))
                 :title             (.getTitle f)
                 :uri               (.getUri f)
                 :block             (some-> itunes-info .getBlock)
                 :explicit          (some-> itunes-info .getExplicit)
                 :subtitle          (some-> itunes-info .getSubtitle)
                 :keywords          (some-> itunes-info .getKeywords seq)
                 :summary           (some-> itunes-info .getSummary)
                 :type              (some-> itunes-info .getType)
                 :complete          (some-> itunes-info .getComplete)
                 :itunes-categories (some->> itunes-info .getCategories (mapcat category->categories))})))

(defn- parse-internal [xmlreader]
  (let [feedinput (new SyndFeedInput)
        syndfeed  (.build feedinput xmlreader)]
    (obj->feed syndfeed)))

(defn parse-stream
  [input-stream]
  (parse-internal (XmlReader. input-stream)))

(defn ->url [s]
  (if (string? s) (URL. s) s))

(defn parse-feed "Get and parse a feed from a URL"
  ([feedsource]
   (parse-internal (XmlReader. (->url feedsource))))
  ([feedsource content-type]
   (parse-internal (XmlReader. (->url feedsource) content-type)))
  ([feedsource content-type lenient]
   (parse-internal (XmlReader. (->url feedsource) content-type lenient)))
  ([feedsource content-type lenient default-encoding]
   (parse-internal (XmlReader. (->url feedsource) content-type lenient default-encoding))))
