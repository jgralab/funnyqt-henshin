(ns funnyqt-henshin.core-test
  (:require [clojure.test :refer :all]
            [funnyqt-henshin.core :refer :all]
            [funnyqt.query :as q]
            [funnyqt.emf :as emf]
            [funnyqt.visualization :as viz]))

;;# Tests

;;## The Bank Example

(henshin-to-funnyqt "examples/bank/" "bank.henshin"
                    funnyqt-henshin.core-test.examples.bank
                    bank)

(emf/load-ecore-resource "examples/bank/bank.ecore")

(defn bank-create-sample-model []
  (let [r (emf/new-resource)
        b (emf/ecreate! r 'Bank)
        m (emf/ecreate! r 'Manager {:name "Mike Manager"})
        c1 (emf/ecreate! r 'Client {:name "Lisa Client"})
        c2 (emf/ecreate! r 'Client {:name "Mona Client"})
        c3 (emf/ecreate! r 'Client {:name "Natasha Client"})
        c4 (emf/ecreate! r 'Client {:name "Olga Client"})]
    (emf/eadd! b :managers m)
    (emf/eadd! b :clients c1 c2 c3 c4)
    (emf/eadd! m :clients c1 c2 c3 c4)
    r))

(deftest test-bank
  (let [g (bank-create-sample-model)
        account (fn [id]
                  (q/the #(= id (emf/eget % :id))
                         (emf/eallcontents g 'Account)))]
    ;; Create the first Account for Lisa.
    (bank/createAccount g "Lisa Client" 1)
    (is (= 1 (count (emf/eallcontents g 'Account))))
    ;; Try to create another Account for her with the same id.  That shouldn't
    ;; work.
    (bank/createAccount g "Lisa Client" 1)
    (is (= 1 (count (emf/eallcontents g 'Account))))
    ;; Try to create another Account for Mona.  That also shouldn't work
    ;; because id 1 is already assigned.
    (bank/createAccount g "Mona Client" 1)
    (is (= 1 (count (emf/eallcontents g 'Account))))
    ;; Now create an Account for Mona with the non-assigned id 2.
    (bank/createAccount g "Mona Client" 2)
    (is (= 2 (count (emf/eallcontents g 'Account))))
    ;; Transfer 10 EUR from Lisa to Mona.
    (bank/transferMoney g "Lisa Client" 1 2 10)
    ;; Now Lisa's Account 1 should have -10 EUR and Mona's Account 2 should
    ;; have 10 EUR.
    (is (== -10 (emf/eget (account 1) :credit)))
    (is (== 10 (emf/eget (account 2) :credit)))
    ;; Create another 5 Accounts for Lisa.
    (dotimes [i 5]
      (bank/createAccount g "Lisa Client" (+ 3 i)))
    (is (= 7 (count (emf/eallcontents g 'Account))))
    ;; Now delete all Accounts of Lisa.
    (bank/deleteAllAccounts g "Lisa Client")
    (is (= 1 (count (emf/eallcontents g 'Account))))
    ;; Now delete Mona's Account.
    (bank/deleteAllAccounts g "Mona Client")
    (is (= 0 (count (emf/eallcontents g 'Account))))))
