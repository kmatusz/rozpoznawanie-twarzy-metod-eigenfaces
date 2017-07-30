# rozpoznawanie twarzy metodą eigenfaces
Projekt miał na celu implementację rozpoznawania twarzy metodą eigewnfaces. Użyłem bazy danych z tej strony: http://cswww.essex.ac.uk/mv/allfaces/faces94.html
W obróbce zdjęć wykorzystałem bibliotekę EBImage, a także metody PCA i KNN. Model osiągnął wynik 16%, co pomimo znacząco lepszego rezultatu od losowania (było 100 twarzy) nie jest dobrym rezultatem.
Prawdopodobnym powodem jest niwielka liczba egzemplarzy treningowych- każda osoba była rozpoznawana tylko na podstawie 1 zdjęcia.
